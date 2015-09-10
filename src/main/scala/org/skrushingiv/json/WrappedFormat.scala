package org.skrushingiv.json

import play.api.libs.json.{Format,Reads,Writes,JsValue}

/**
 * `WrappedFormat` provides methods that implement the Format interface by forwarding to 
 * `Reads` and `Writes` instances local to the implementing class.
 */
trait WrappedFormat[T] extends Format[T] {
  protected val r: Reads[T]
  protected val w: Writes[T]
  def reads(json:JsValue) = r.reads(json)
  def writes(value:T) = w.writes(value)
}
