package org.skrushingiv.json

import org.skrushingiv.test._
import org.specs2.mutable._
import play.api.libs.json._
import scala.util.Try

class EnumerationCombinatorsSpec extends Specification with ConditionalSkipping {

  // Test enumerations with default names
  object Day extends Enumeration {
    val SUN, MON, TUE, WED, THU, FRI, SAT = Value
  }

  // Test enumerations with custom names
  object Foo extends Enumeration {
    val FOO = Value("foo")
    val BAR = Value("bar")
    val BAZ = Value("baz")
  }

  case class Test(foo:Foo.Value, day:Day.Value)

  implicit val readsDay:Reads[Day.Value] = new EnumerationReads(Day)
  implicit val writesDay:Writes[Day.Value] = new EnumerationWrites
  implicit val readsFoo:Reads[Foo.Value] = new EnumerationReads(Foo)
  implicit val writesFoo:Writes[Foo.Value] = new EnumerationWrites
  implicit val formatTest = Json.format[Test]

  "EnumerationReads" should {

    "correctly handle enumeration string values" in {
      Json.parse("""{ "foo":"bar", "day":"WED" }""").as[Test] === Test(Foo.BAR, Day.WED)
    }

    "correctly handle null values" in {
      Json.parse("""{ "foo":null, "day":"THU" }""").as[Test] === Test(null,Day.THU)
      Json.parse("""{ "foo":"baz", "day":null }""").as[Test] === Test(Foo.BAZ,null)
    }

    "Return an error if the value is undefined" in {
      Json.parse("""{ "day":"THU" }""").as[Test] must throwA[JsResultException]
      Json.parse("""{ "foo":"baz" }""").as[Test] must throwA[JsResultException]
    }

    "Return an error if the value is not an acceptable value" in {
      Json.parse("""{ "foo":"bar", "day":"Zippo" }""").as[Test] must throwA[JsResultException]
      Json.parse("""{ "foo":"Hippo", "day":"SAT" }""").as[Test] must throwA[JsResultException]
    }
  }

  "EnumerationWrites" should {

    val result = Try { Json.toJson(Test(Foo.BAZ, Day.SUN)).asInstanceOf[JsObject] }

    "successfully generate a JSON object" in {
      result.get must not(throwA[Throwable])
    }

    "correctly serialize enumeration values as their name" in skipIf(result isFailure) {
      result.get.value("foo") === JsString("baz")
      result.get.value("day") === JsString("SUN")
    }

    "correctly handle null values" in skipIf(result isFailure) {
      val result = Json.toJson(Test(null, null)).asInstanceOf[JsObject]
      result.value("foo") === JsNull
      result.value("day") === JsNull
    }
  }
}
