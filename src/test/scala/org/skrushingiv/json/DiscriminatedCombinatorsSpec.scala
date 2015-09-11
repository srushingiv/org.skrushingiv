package org.skrushingiv.json

import org.specs2.mutable._
import play.api.libs.json._
import scala.util.Try

class DiscriminatedCombinatorsSpec extends Specification with ConditionalSkipping {

  // This scenario, although rediculous, should be representative of most use cases.
  //
  // The general idea is that more complex types would have a value field that could be any of
  // several subclasses of some super-class or trait.

  object Type extends Enumeration {
    val INT = Value("int")
    val STR = Value("str")
    val BAD = Value("bad")
  }

  sealed trait TypedValue
  case class IntValue(integer:Int) extends TypedValue
  case class StrValue(string:String) extends TypedValue
  case object BadValue extends TypedValue // this is for testing writes 

  implicit val formatType:Format[Type.Value] = new EnumerationFormat(Type)
  implicit val formatIntValue = Json.format[IntValue]
  implicit val readsStrValue = Json.reads[StrValue]
  implicit val writesStrValue = Json.writes[StrValue]

  implicit val formatTypeValue = new DiscriminatedFormat[Type.Value, TypedValue]( JsPath \ "type",
      Type.INT -> classOf[IntValue],
      Type.STR -> classOf[StrValue]
  )

  "DiscriminatedReads" should {

    "correctly parse values of each subtype" in {
      Json.parse("""{ "type":"int", "integer":123 }""").as[TypedValue] === IntValue(123)
      Json.parse("""{ "type":"str", "string":"abc" }""").as[TypedValue] === StrValue("abc")
    }

    "fail when an unexpected discriminator is encountered" in {
      Json.parse("""{ "type":"bad", "foo":"bar" }""").as[TypedValue] must throwA[JsResultException]
    }

    "fail when the discriminator value cannot be read" in {
      Json.parse("""{ "type":"very bad", "foo":"bar" }""").as[TypedValue] must throwA[JsResultException]
    }
  }

  "DiscriminatedWrites" should {

    val result1 = Try { Json.toJson(IntValue(123).asInstanceOf[TypedValue]).asInstanceOf[JsObject] }
    val result2 = Try { Json.toJson(StrValue("abc").asInstanceOf[TypedValue]).asInstanceOf[JsObject] }
    val result3 = Try { Json.toJson(BadValue.asInstanceOf[TypedValue]).asInstanceOf[JsObject] }

    "successfully generate a JSON object" in {
      result1.get must not(throwA[Throwable])
      result2.get must not(throwA[Throwable])
    }

    "fail when trying to write a sub-type that is not expected" in {
      result3.get must throwA[NoSuchElementException]
    }

    "correctly serialize values of each subtype" in skipIf(result1.isFailure || result2.isFailure) {
      result1.get.value("type") === JsString("int")
      result2.get.value("type") === JsString("str")
      result1.get.value("integer") === JsNumber(123)
      result2.get.value("string") === JsString("abc")
    }
  }
}
