package org.skrushingiv.json

import play.api.libs.json._
import scala.util.Try
import org.skrushingiv.util.TryUtils

/**
 * An EnumerationReads object attempts to transform a JSON String into an Enumerated value
 * by name. The read will result in a JsError if the value is not a string, or the value cannot
 * be found in the enumeration.
 */
class EnumerationReads[E <: Enumeration](enum: E) extends Reads[E#Value] {
  private def error(s:String,fail:Throwable) = {
    val values = enum.values.map(_.toString).mkString("\"","\", \"","\"")
    JsError("Expected one of ["+values+"] but encountered \""+s+"\".")
  }

  private def string2value(s:String) = Try(enum.withName(s)).fold( JsSuccess(_), error(s,_))

  def reads(json:JsValue): JsResult[E#Value] =
    if (json == JsNull) JsSuccess(null) // handle null values without errors
    else Reads.StringReads reads json flatMap string2value // handle non-null values
}

/**
 * An EnumerationWrites object writes enumeration values using their string representation.
 */
class EnumerationWrites[E <: Enumeration] extends Writes[E#Value] {
  def writes(value:E#Value):JsValue = if (value == null) JsNull else JsString(value.toString)
}

/**
 * An EnumerationFormat object reads and writes enumeration values using their string representation.
 */
class EnumerationFormat[E <: Enumeration](enum: E) extends WrappedFormat[E#Value] {
  protected val r = new EnumerationReads(enum)
  protected val w = new EnumerationWrites[E]
}
