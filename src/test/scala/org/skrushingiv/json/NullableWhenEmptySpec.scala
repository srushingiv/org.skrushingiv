package org.skrushingiv.json

import org.specs2.mutable._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.util.Try

class NullableWhenEmptySpec extends Specification {

  case class Test(foo:String, bar:List[Int] = List.empty, baz:List[Test] = List.empty)

  case class Test2(fee:Double, fie:Map[String,Test] = Map.empty, foe:Map[String,Test2] = Map.empty)

  implicit val readsTest:Reads[Test] = (
      (__ \ "foo").read[String] ~
      (__ \ "bar").readNullableWhenEmpty[List[Int]] ~
      (__ \ "baz").lazyReadNullableWhenEmpty(Reads.list(readsTest))
  )(Test.apply _)

  implicit val writesTest:Writes[Test] = (
      (__ \ "foo").write[String] ~
      (__ \ "bar").writeNullableWhenEmpty[List[Int]] ~
      (__ \ "baz").lazyWriteNullableWhenEmpty(Writes.list(writesTest))
  )(unlift(Test.unapply))

  implicit val readsTestMapFormat = Reads.mapReads(readsTest)
  implicit val writesTestMap = Writes.mapWrites(writesTest)

  implicit val readsTest2:Reads[Test2] = (
      (__ \ "fee").read[Double] ~
      (__ \ "fie").readNullableWhenEmpty[Map[String,Test]] ~
      (__ \ "foe").lazyReadNullableWhenEmpty(Reads.map(readsTest2))
  )(Test2.apply _)

  implicit val writesTest2:Writes[Test2] = (
      (__ \ "fee").write[Double] ~
      (__ \ "fie").writeNullableWhenEmpty[Map[String,Test]] ~
      (__ \ "foe").lazyWriteNullableWhenEmpty(Writes.map(writesTest2))
  )(unlift(Test2.unapply))

  "\"readNullableWhenEmpty\" and \"lazyReadNullableWhenEmpty\" for Lists and recursive Lists" should {

    val listJson = Json.parse("""{
      "foo":"a",
      "baz":[
        {
          "foo":"b",
          "bar":[1, 2]
        },{
          "foo":"c",
          "bar":[3, 4, 5],
          "baz":null
        },{
          "foo":"d",
          "bar":null,
          "baz":[]
        }
      ]
    }""")

    val result:Try[Test] = Try { listJson.as[Test] }

    "successfully transform a valid json object" in {
      result.get should not(throwA[Throwable])
    }

    "correctly read lists with elements" in {
      result.get.baz.length === 3
      result.get.baz(0).bar === List(1,2)
      result.get.baz(1).bar === List(3,4,5)
    }

    "read missing, null, or empty arrays as empty lists" in {
      result.get.bar === List.empty
      result.get.baz(2).bar === List.empty
      result.get.baz(0).baz === List.empty
      result.get.baz(1).baz === List.empty
      result.get.baz(2).baz === List.empty
    }
  }

  "\"writeNullableWhenEmpty\" and \"lazyWriteNullableWhenEmpty\" for Lists and recursive Lists" should {

    val test = Test("a",baz = List(Test("b",List(1,2)),Test("c",List(3,4,5)),Test("d")))

    val result:Try[(JsObject,JsArray)] = Try {
      val obj = Json.toJson(test).asInstanceOf[JsObject]
      val arr = obj.fieldSet.filter( _._1 == "baz" ).headOption.map(_._2).get.asInstanceOf[JsArray]
      (obj,arr)
    }

    "successfully transform into a valid json object" in {
      result.get should not(throwA[Throwable])
    }

    "correctly write empty and non-empty lists" in {
      val (obj,arr) = result.get
      val resultKeys = obj.fieldSet map (_._1)
      resultKeys must contain("foo")
      resultKeys must not contain("bar")
      resultKeys must contain("baz")

      val arrKeys = arr.value.map(_.asInstanceOf[JsObject].fieldSet.map(_._1))
      arrKeys(0) must contain("foo")
      arrKeys(0) must contain("bar")
      arrKeys(0) must not contain("baz")
      arrKeys(1) must contain("foo")
      arrKeys(1) must contain("bar")
      arrKeys(1) must not contain("baz")
      arrKeys(2) must contain("foo")
      arrKeys(2) must not contain("bar")
      arrKeys(2) must not contain("baz")
    }
  }

  // TODO: Add unit tests for Maps
}

