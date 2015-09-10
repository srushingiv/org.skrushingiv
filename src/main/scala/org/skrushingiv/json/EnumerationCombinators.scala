package org.skrushingiv.json

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.data.validation.ValidationError
import scala.util.{Try, Success}

/**
 * An EnumerationReads object attempts to transform a JSON String into an Enumerated value
 * by name. The read will result in a JsError if the value is not a string, or the value cannot
 * be found in the enumeration.
 */
class EnumerationReads[E <: Enumeration](enum: E) extends Reads[E#Value] {
  def reads(json:JsValue): JsResult[E#Value] = Reads.StringReads.reads(json).flatMap { s =>
    Try(enum.withName(s)) match {
      case Success(e) => JsSuccess(e)
      case _ =>
        val values = enum.values.map(_.toString).mkString("\"","\", \"","\"")
        JsError("Expected one of "+values+" but encountered \""+s+"\".")
    }
  }
}

/**
 * An EnumerationWrites object writes enumeration values using their string representation.
 */
class EnumerationWrites[E <: Enumeration] extends Writes[E#Value] {
  def writes(value:E#Value):JsValue = JsString(value.toString)
}

/**
 * An EnumerationFormat object reads and writes enumeration values using their string representation.
 */
class EnumerationFormat[E <: Enumeration](enum: E) extends WrappedFormat[E#Value] {
  protected val r = new EnumerationReads(enum)
  protected val w = new EnumerationWrites[E]
}
