package org.skrushingiv.json

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError
import scala.reflect.ClassTag

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
 * An example of this type-hierarchy support can be seen in [ies.common.model.scheduledevent.DREventJSONConverters]
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
    dpReads.reads(json).get match {
      case None | Some(_:JsUndefined) =>
        JsError(discriminatorPath, ValidationError("Discriminator value missing."))
      case Some(d) if options.get(d).isEmpty =>
        JsError(discriminatorPath, ValidationError("Discriminator value does not match any expected value."))
      case Some(d) => options(d).reads(json)
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
 *     trait Foo { def discriminator:String }
 *     case class Bar extends Foo { val discriminator = "bar" }
 *     case class Baz extends Foo { val discriminator = "baz" }
 * 
 * We can create a generic Writes[Foo[_]] with:
 *     implicit val barWrites = Json.writes[Bar]
 *     implicit val bazWrites = Json.writes[Baz]
 *     implicit val fooWrites = new DiscriminatedWrites(__ \ "discriminator", Map("bar" -> (classOf[Bar], barWrites), "baz" -> (classOf[Baz], bazWrites)))
 * 
 * This generic `Writes[Foo[_]]` will write a `Bar` object as `{ "discriminator" : "bar" }`.
 * 
 * An example of this type-hierarchy support can be seen in [ies.common.model.scheduledevent.DREventJSONConverters]
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

  private val dpWrites = discriminatorPath.write(dWrites)

  // generate a map of runtime classes to writers that output both the sub-class JSON and the discriminator.
  private val map:Seq[(Class[_ <: T],Writes[T])] = options.map { case (d, cw) =>
    (cw._1 -> cw._2.transform({ json => json.asInstanceOf[JsObject] ++ dpWrites.writes(d) }).asInstanceOf[Writes[T]])
  }

  def writes(value: T): JsValue = {
    val vc = value.getClass
    map.collectFirst { case (c,w) if c.isAssignableFrom(vc) => w } match {
      case Some(writer) =>
        writer.writes(value)
      case None =>
        throw new NoSuchElementException("No Writes available for "+vc.getName+" as a discriminated sub-class of "+ct.runtimeClass.getName)
    }
  }
}
