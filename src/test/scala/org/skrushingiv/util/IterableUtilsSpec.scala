package org.skrushingiv.util

import org.specs2.mutable._

class IterableUtilsSpec extends Specification {

  val testMap = Map("a" -> 1, "b" -> 2, "c" -> 3)
  val testList = List("a", "b", "c")
  val testSeq = Seq(1,2,3)

  var streamCounter = 0;
  def testStreamGen(i:Int):Stream[Int] = {
    streamCounter += 1
    i #:: testStreamGen(i+1)
  }
  val testStream = testStreamGen(0)

  "\"ifDefined\" on an empty collection" should {

    def test[T <: Iterable[_]](name:String, collection:T) = ("return None for "+name) in {
      collection.ifDefined(_ => Unit) === None
    }

    test("Maps", Map.empty[Any,Any])
    test("Lists", List.empty[Any])
    test("Seqs", Seq.empty[Any])
    test("Streams", Stream.empty[Any])
  }

  "\"ifDefined\" on a non-empty collection" should {

    def test[T <: Iterable[_]](name:String, collection:T) = ("return Some for "+name) in {
      collection.ifDefined(identity) === Some(collection)
      streamCounter === 1 // ensure that the stream is not explored past the head element.
    }

    test("Maps", testMap)
    test("Lists", testList)
    test("Seqs", testSeq)
    test("Streams", testStream)
  }

  "\"ifEmpty\" on an empty collection" should {

    val x = 1337
    def test[T <: Iterable[_]](name:String, collection:T) = ("return Some(value) for "+name) in {
      collection.ifEmpty(x) === Some(1337)
    }

    test("Maps", Map.empty[Any,Any])
    test("Lists", List.empty[Any])
    test("Seqs", Seq.empty[Any])
    test("Streams", Stream.empty[Any])
  }

  "\"ifEmpty\" on a non-empty collection" should {

    val foo = "bar"
    def test[T <: Iterable[_]](name:String, collection:T) = ("return None for "+name) in {
      collection.ifEmpty(foo) === None
      streamCounter === 1 // ensure that the stream is not explored past the head element.
    }

    test("Maps", testMap)
    test("Lists", testList)
    test("Seqs", testSeq)
    test("Streams", testStream)
  }

  "\"onEmpty\" on an empty collection" should {

    def test[T <: Iterable[_]](name:String, collection:T) = ("perform the side effect for "+name) in {
      var y = 0
      collection.onEmpty(y += 1)
      y === 1
    }

    test("Maps", Map.empty[Any,Any])
    test("Lists", List.empty[Any])
    test("Seqs", Seq.empty[Any])
    test("Streams", Stream.empty[Any])
  }

  "\"onEmpty\" on a non-empty collection" should {

    val foo = "bar"
    def test[T <: Iterable[_]](name:String, collection:T) = ("not perform the side effect for "+name) in {
      var y = 0
      collection.onEmpty(y += 1)
      y === 0
      streamCounter === 1 // ensure that the stream is not explored past the head element.
    }

    test("Maps", testMap)
    test("Lists", testList)
    test("Seqs", testSeq)
    test("Streams", testStream)
  }
}

