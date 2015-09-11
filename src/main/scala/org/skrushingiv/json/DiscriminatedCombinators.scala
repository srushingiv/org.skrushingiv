package org.skrushingiv.json

import play.api.libs.json._
import scala.reflect.ClassTag
import scala.language.existentials

/**
 * A DiscriminatedReads object uses a "discriminator value" at a specified JSON path to determine which
 * concrete Reads to use for a hierarchy of sub-classes. The discriminator value can be of any readable
 * type. All concrete Reads in the provided options map should read subclasses of a common super-type.
 * 
 * For instance, given the type hyerarchy:
 *     trait Foo { def discriminator:String }
 *     case class Bar extends Foo { val discriminator = "bar" }
 *     case class Baz extends Foo { val discriminator = "baz" }
 * 
 * We can create a generic Reads[Foo[_]] with:
 *     implicit val barReads = Json.reads[Bar]
 *     implicit val bazReads = Json.reads[Baz]
 *     implicit val fooReads = new DiscriminatedReads(__ \ "discriminator", Map("bar" -> barReads, "baz" -> bazReads))
 * 
 * This generic `Reads[Foo[_]]` will read `{ "discriminator" : "bar" }` as a `Bar` object.
 * 
 * @param discriminatorPath The JsPath describing the location of the discriminator value.
 * @param options A collection of discriminator values and matching Reads for concrete subclasses.
 * @param dFormat The Reads to use for the discriminator value - Most-often will be something implicitly resolved like `Reads.stringReads`
 */
class DiscriminatedReads[D, T](discriminatorPath:JsPath,
    // The entries in the options list are not simply Reads[_ <: T] because _ resolves at the top-level.
    // What we are `(D, Reads[SubT] forSome {type SubT <: T}) *` means is a Seq of discriminator values
    //     paired with a reader for a sub-type of T where the sub-types in each element are not related.
    // But `(D, Reads[_ <: T]) *` would require that all Reads in the list read the same sub-type of T -
    //     which is NOT what we want.
    options:Map[D, Reads[SubT] forSome {type SubT <: T}])(implicit dReads:Reads[D]) extends Reads[T] {

  def this(discriminatorPath:JsPath, options:(D, Reads[SubT] forSome {type SubT <: T}) *)(implicit dReads:Reads[D]) =
    this(discriminatorPath, Map(options:_*))

  private val dpReads = discriminatorPath.readNullable(dReads)

  def reads(json: JsValue): JsResult[T] = {
    dpReads.reads(json) match {
      case e:JsError => e
      case JsSuccess(None | Some(_:JsUndefined), path) =>
        JsError(path, "Discriminator value missing.")
      case JsSuccess(Some(d),path) if options.get(d).isEmpty =>
        JsError(path, "Discriminator value does not match any expected value.")
      case JsSuccess(Some(d),_) => options(d).reads(json)
    }
  }
}

/**
 * A DiscriminatedWrites object uses a "discriminator value" at a specified JSON path to indicate which
 * concrete sub-class in a type-hierarchy that de-serializers should use. The discriminator value can be
 * of any writeable type. All concrete Writes in the provided options map should write subclasses of a common
 * super-type.
 * 
 * For instance, given the type hyerarchy:
 *     trait Foo
 *     case class Bar extends Foo
 *     case class Baz extends Foo
 * 
 * We can create a generic Writes[Foo] with:
 *     implicit val barWrites = Json.writes[Bar]
 *     implicit val bazWrites = Json.writes[Baz]
 *     implicit val fooWrites = new DiscriminatedWrites[String,Foo](__ \ "discriminator", Map("bar" -> classOf[Bar], "baz" -> classOf[Baz]))
 * 
 * This generic `Writes[Foo]` will write a `Bar` object as `{ "discriminator" : "bar" }`.
 * 
 * @param discriminatorPath The JsPath describing the location of the discriminator value.
 * @param options A Map of discriminator values to Class and Writes for concrete subclasses.
 * @param dFormat The Writes to use for the discriminator value - Most-often will be something implicitly resolved like `Writes.stringWrites`
 */
class DiscriminatedWrites[D, T : ClassTag](discriminatorPath:JsPath,
    // The entry values in the options array are organized in this way to ensure that each entry
    // may refer to a different "SubT" sub-type class but that both the Class and the Writes value refer to the same SubT type.
    options:(D, (Class[SubT], Writes[SubT]) forSome {type SubT <: T}) *)(implicit dWrites:Writes[D]) extends Writes[T] {

  private val ct:ClassTag[T] = implicitly // So we can output meaningful error messages

  // writes discriminator values at the specified discriminator path
  private val dpWrites = discriminatorPath.write(dWrites)

  // generate a map of runtime classes to writers that output both the sub-class JSON and the discriminator.
  private val map:Seq[(Class[_ <: T],Writes[T])] = options.map { case (d, cw) =>
    (cw._1 -> cw._2.transform({ json => json.asInstanceOf[JsObject] ++ dpWrites.writes(d) }).asInstanceOf[Writes[T]])
  }

  def writes(value: T): JsValue = {
    val vc = value.getClass // this is the runtime sub-class of T - the class of the concrete value. 
    map.collectFirst { case (c,w) if c.isAssignableFrom(vc) => w } map (_.writes(value)) getOrElse {
      throw new NoSuchElementException("No Writes available for "+vc.getName+" as a discriminated sub-class of "+ct.runtimeClass.getName)
    }
  }
}

/**
 * Will generate a Format object that uses a "discriminator value" at a specified JSON path to indicate which
 * concrete sub-class in a type-hierarchy that serializers and de-serializers should use. The discriminator
 * value can be of any format-able type. All concrete Formats in the provided options map should format
 * subclasses of a common super-type.
 * 
 * For instance, given the type hyerarchy:
 *     trait Foo
 *     case class Bar extends Foo
 *     case class Baz extends Foo
 * 
 * We can create a generic Format[Foo] with:
 *     implicit val barFormat = Json.format[Bar]
 *     implicit val bazFormat = Json.format[Baz]
 *     implicit val fooFormat = DiscriminatedFormat(__ \ "discriminator", Map("bar" -> classOf[Bar], "baz" -> classOf[Baz]))
 * 
 * This generic `Format[Foo]` will write a `Bar` object as `{ "discriminator" : "bar" }` and
 * will read `{ "discriminator" : "baz" }` as a `Baz` object.
 * 
 * @param discriminatorPath The JsPath describing the location of the discriminator value.
 * @param options A Map of discriminator values to Class and Formatters for concrete subclasses.
 * @param dReads The reader to use for the discriminator value - Most-often will be something implicitly resolved like `Reads.stringReads`
 * @param dWrites The writer to use for the discriminator value - Most-often will be something implicitly resolved like `Writes.stringWrites`
 */
class DiscriminatedFormat[D, T : ClassTag](discriminatorPath:JsPath, options:(D, (Class[SubT], Format[SubT]) forSome {type SubT <: T}) *)
    (implicit dReads:Reads[D], dWrites:Writes[D]) extends WrappedFormat[T] {
  protected val r = new DiscriminatedReads[D, T](discriminatorPath, options.map(x => (x._1, x._2._2)):_*)
  protected val w = new DiscriminatedWrites[D, T](discriminatorPath, options:_*)
}

