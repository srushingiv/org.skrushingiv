package org.skrushingiv

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError
import scala.collection.MapLike
import scala.reflect.ClassTag

package object json {

  implicit class PathAdditions(val path: JsPath) extends AnyVal {

    /**
     * Determines the correct empty JsValue to use for a collection type.
     */
    private def emptyValue[A](implicit evidence: A <:< MapLike[_,_,_] = null): JsValue =
      if (evidence == null) JsArray() else Json.obj()

    /**
     * When an undefined or null value is encountered, returns an empty collection of the specified type.
     * 
     * This differs from the standard de-serializers provided by the play api in that a missing value is
     * deserialized as an empty collection, instead of needing to wrap the collection in an option.
     */
    def readNullableWhenEmpty[A <: Iterable[_]](implicit r: Reads[A], evidence: A <:< MapLike[_,_,_] = null) = Reads[A] { json =>
      path.applyTillLast(json).fold(
        error => error,
        result => result.fold(
          invalid = (_) => r.reads(emptyValue[A]),
          valid = {
            case JsNull => r.reads(emptyValue[A])
            case js => r.reads(js).repath(path)
          })
      )
    }

    /**
     * When an empty collection is provided, does not emit the path.
     * 
     * This differs from the standard serializers provided by the play api in that there is no need to
     * wrap the collection in an Option in order to omit the key in the serialized representation.
     */
    def writeNullableWhenEmpty[A <: Iterable[_]](implicit w: Writes[A]) = OWrites[A] { a =>
      if (a.isEmpty) Json.obj()
      else JsPath.createObj((path, w.writes(a)))
    }

    /**
     * When writing it ignores the property when the collection is empty,
     * when reading undefined and empty jsarray becomes an empty collection
     */
    def formatNullableWhenEmpty[A <: Iterable[_]](implicit r: Reads[A], w: Writes[A], evidence: A <:< MapLike[_,_,_] = null): OFormat[A] =
      OFormat[A](readNullableWhenEmpty[A], writeNullableWhenEmpty[A])

  }

  /**
   * Will generate a Format object that uses a "discriminator value" at a specified JSON path to indicate which
   * concrete sub-class in a type-hierarchy that serializers and de-serializers should use. The discriminator
   * value can be of any format-able type. All concrete Formats in the provided options map should format
   * subclasses of a common super-type.
   * 
   * For instance, given the type hyerarchy:
   *     trait Foo { def discriminator:String }
   *     case class Bar extends Foo { val discriminator = "bar" }
   *     case class Baz extends Foo { val discriminator = "baz" }
   * 
   * We can create a generic Format[Foo[_]] with:
   *     implicit val barFormat = Json.format[Bar]
   *     implicit val bazFormat = Json.format[Baz]
   *     implicit val fooFormat = DiscriminatedFormat(__ \ "discriminator", Map("bar" -> (classOf[Bar], barFormat), "baz" -> (classOf[Baz], bazFormat)))
   * 
   * This generic `Format[Foo[_]]` will write a `Bar` object as `{ "discriminator" : "bar" }` and
   * will read `{ "discriminator" : "baz" }` as a `Baz` object.
   * 
   * An example of this type-hierarchy support can be seen in [ies.common.model.scheduledevent.DREventJSONConverters]
   * 
   * @param discriminatorPath The JsPath describing the location of the discriminator value.
   * @param options A Map of discriminator values to Class and Formatters for concrete subclasses.
   * @param dReads The reader to use for the discriminator value - Most-often will be something implicitly resolved like `Reads.stringReads`
   * @param dWrites The writer to use for the discriminator value - Most-often will be something implicitly resolved like `Writes.stringWrites`
   */
  def DiscriminatedFormat[D, T : ClassTag](discriminatorPath:JsPath, options:(D, (Class[SubT], Format[SubT]) forSome {type SubT <: T}) *)
      (implicit dReads:Reads[D], dWrites:Writes[D]): Format[T] = {
    val r = new DiscriminatedReads[D, T](discriminatorPath, options.map(x => (x._1, x._2._2)):_*)
    val w = new DiscriminatedWrites[D, T](discriminatorPath, options:_*)
    Format(r, w)
  }

  /**
   * Will generate a Format object for Enumeration values that use the value names as the
   * serialized representation of the value.
   */
  def EnumerationFormat[E <: Enumeration](enum: E) = Format(new EnumerationReads(enum), new EnumerationWrites[E])
}
