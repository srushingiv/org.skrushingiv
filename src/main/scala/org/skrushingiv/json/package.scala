package org.skrushingiv

import play.api.libs.json._
import scala.collection.MapLike

package object json {

  implicit class PathAdditions(val path: JsPath) extends AnyVal {

    /**
     * Determines the correct empty JsValue to use for a collection type.
     * 
     * @param evidence Evidence that the parameter class `A` is a `Map`, use `null` if not. This value is actually not
     *                 provided by the compiler here, but in the two calling methods `readNullableWhenEmpty` and
     *                 `formatNullableWhenEmpty` instead. If not provided in those methods, it would be eliminated
     *                 by erasure.
     */
    private def emptyValue[A](implicit evidence: A <:< MapLike[_,_,_]): JsValue =
      if (evidence == null) JsArray() else Json.obj()

    /**
     * When an undefined or null value is encountered, returns an empty collection of the specified type.
     * 
     * This differs from the standard de-serializers provided by the play api in that a missing value is
     * deserialized as an empty collection, instead of needing to wrap the collection in an option.
     * 
     * @param evidence Compiler-provided evidence that the parameter class `A` is a `Map`, or `null`.
     * @param r A `Reads` for the collection type.
     */
    def readNullableWhenEmpty[A <: Traversable[_]](implicit r: Reads[A], evidence: A <:< MapLike[_,_,_] = null) =
      lazyReadNullableWhenEmpty(r)(evidence)

    def lazyReadNullableWhenEmpty[A <: Traversable[_]](r: => Reads[A])(implicit evidence: A <:< MapLike[_,_,_] = null) = Reads[A] { json =>
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
     * 
     * @param w A `Writes` for the collection type.
     */
    def writeNullableWhenEmpty[A <: Traversable[_]](implicit w: Writes[A]) =
      lazyWriteNullableWhenEmpty(w)

    def lazyWriteNullableWhenEmpty[A <: Traversable[_]](w: => Writes[A]) = OWrites[A] { a =>
      if (a.isEmpty) Json.obj()
      else JsPath.createObj((path, w.writes(a)))
    }

    /**
     * When writing it ignores the property when the collection is empty,
     * when reading undefined and empty jsarray becomes an empty collection
     * 
     * @param evidence Compiler-provided evidence that the parameter class `A` is a `Map`, or `null`.
     * @param r A `Reads` for the collection type.
     * @param w A `Writes` for the collection type.
     */
    def formatNullableWhenEmpty[A <: Traversable[_]](implicit r: Reads[A], w: Writes[A], evidence: A <:< MapLike[_,_,_] = null): OFormat[A] =
      OFormat[A](readNullableWhenEmpty[A], writeNullableWhenEmpty[A])

  }

}
