package org.skrushingiv.json

import org.specs2.mutable._
import org.specs2.execute.AsResult
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.util.Try

class NullableWhenEmptySpec extends Specification with ConditionalSkipping {

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

  val listJson = """{
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
  }"""

  val mapJson = """{
    "fee":3.1415,
    "foe":{
      "a":{
        "fee":6.022,
        "fie":{
          "d":{"foo":"d"},
          "e":{"foo":"e"}
        },
        "foe":null
      },
      "b":{
        "fee":2.54,
        "foe":{}
      },
      "c":{
        "fee":521600
      }
    }
  }"""

  val listTest = Test("a",baz = List(Test("b",List(1,2)),Test("c",List(3,4,5)),Test("d")))

  val mapTest = Test2(3.1415,foe = Map(
      "a" -> Test2(6.022, fie = Map("d" -> Test("d"), "e" -> Test("e"))),
      "b" -> Test2(2.54),
      "c" -> Test2(521600)
    ))


  "\"readNullableWhenEmpty\" and \"lazyReadNullableWhenEmpty\" for Lists and recursive Lists" should {

    val result:Try[Test] = Try { Json.parse(listJson).as[Test] }

    "successfully transform a valid json object" in {
      result.get should not(throwA[Throwable])
    }

    "correctly read lists with elements" in skipIf(result isFailure) {
      result.get.baz.length === 3
      result.get.baz(0).bar === List(1,2)
      result.get.baz(1).bar === List(3,4,5)
    }

    "read missing, null, or empty arrays as empty lists" in skipIf(result isFailure) {
      result.get.bar === List.empty
      result.get.baz(2).bar === List.empty
      result.get.baz(0).baz === List.empty
      result.get.baz(1).baz === List.empty
      result.get.baz(2).baz === List.empty
    }
  }

  "\"writeNullableWhenEmpty\" and \"lazyWriteNullableWhenEmpty\" for Lists and recursive Lists" should {

    val result:Try[(JsObject,JsArray)] = Try {
      val obj = Json.toJson(listTest).asInstanceOf[JsObject]
      val arr = obj.fieldSet.filter( _._1 == "baz" ).headOption.map(_._2).get.asInstanceOf[JsArray]
      (obj,arr)
    }

    "successfully transform into a valid json object" in {
      result.get should not(throwA[Throwable])
    }

    "correctly write empty and non-empty lists" in skipIf(result isFailure) {
      val (obj,arr) = result.get
      val resultKeys = obj.fieldSet map (_._1)
      resultKeys must contain("foo")
      resultKeys must not contain("bar")
      resultKeys must contain("baz")

      val arrKeys = arr.value.map(_.asInstanceOf[JsObject].fieldSet.map(_._1))
      arrKeys(0) must containAllOf(Seq("foo","bar"))
      arrKeys(0) must not contain("baz")
      arrKeys(1) must containAllOf(Seq("foo","bar"))
      arrKeys(1) must not contain("baz")
      arrKeys(2) must contain("foo")
      arrKeys(2) must not (containAnyOf(Seq("bar","baz")))
    }
  }

  "\"readNullableWhenEmpty\" and \"lazyReadNullableWhenEmpty\" for Maps and recursive Maps" should {

    val result:Try[Test2] = Try { Json.parse(mapJson).as[Test2] }

    "successfully transform a valid json object" in {
      result.get should not(throwA[Throwable])
    }

    "correctly read maps with elements" in skipIf(result isFailure) {
      result.get.foe.size === 3
      result.get.foe.keySet must containAllOf(Seq("a","b","c"))
      result.get.foe("a").fie.size === 2
      result.get.foe("a").fie.keySet must containAllOf(Seq("d","e"))
    }

    "read missing, null, or empty objects as empty maps" in skipIf(result.isFailure || result.get.foe.size != 3) {
      result.get.fie === Map.empty // missing
      result.get.foe("a").foe === Map.empty // null
      result.get.foe("b").fie === Map.empty // missing
      result.get.foe("b").foe === Map.empty // empty
      result.get.foe("c").fie === Map.empty // missing
      result.get.foe("c").foe === Map.empty // missing
    }
  }

  "\"writeNullableWhenEmpty\" and \"lazyWriteNullableWhenEmpty\" for Maps and recursive Maps" should {

    val result:Try[(JsObject,JsObject)] = Try {
      val obj = Json.toJson(mapTest).asInstanceOf[JsObject]
      val map = obj.fieldSet.filter( _._1 == "foe" ).headOption.map(_._2).get.asInstanceOf[JsObject]
      (obj,map)
    }

    "successfully transform into a valid json object" in {
      result.get should not(throwA[Throwable])
    }

    "correctly write empty and non-empty maps" in skipIf(result isFailure) {
      val (obj,map) = result.get
      val resultKeys = obj.fieldSet map (_._1)
      resultKeys must contain("fee")
      resultKeys must not contain("fie")
      resultKeys must contain("foe")

      val mapKeys = map.value.map { case (k,v) => (k,v.asInstanceOf[JsObject].fieldSet.map(_._1)) }
      mapKeys("a") must containAllOf(Seq("fee","fie"))
      mapKeys("a") must not contain("foe")
      mapKeys("b") must contain("fee")
      mapKeys("b") must not (containAnyOf(Seq("fie","foe")))
      mapKeys("c") must contain("fee")
      mapKeys("c") must not (containAnyOf(Seq("fie","foe")))
    }
  }

  "\"readNullableWhenEmpty\" and \"writeNullableWhenEmpty\"" should {

    "deserialize the same value from a serialized value for Lists" in {
      val str = Json.toJson(listTest).toString
      val result = Json.parse(str).as[Test]
      result === listTest
    }

    "deserialize the same value from a serialized value for Maps" in {
      val str = Json.toJson(mapTest).toString
      val result = Json.parse(str).as[Test2]
      result === mapTest
    }
  }
}

