package org.skrushingiv.util

import org.specs2.mutable._

class EnumerationNameExtractorSpec extends Specification {

  // Test enumerations with default names
  object Day extends Enumeration with EnumerationNameExtractor {
    val SUN, MON, TUE, WED, THU, FRI, SAT = Value
  }

  // Test enumerations with custom names
  object Foo extends Enumeration with EnumerationNameExtractor {
    val FOO = Value("foo")
    val BAR = Value("bar")
    val BAZ = Value("baz")
  }

  "\"apply\"" should {

    "return known values" in {
      Day("MON") === Day.MON
      Day("THU") === Day.THU
      Foo("bar") === Foo.BAR
    }

    "throw an exception for unknown values" in {
      Day("Saturday") must throwA[NoSuchElementException]
      Day("Fraggles Rock!") must throwA[NoSuchElementException]
      Foo("fum") must throwA[NoSuchElementException]
    }

    "throw an exception for null values" in {
      Day(null) must throwA[NoSuchElementException]
      Foo(null) must throwA[NoSuchElementException]
    }
  }

  "\"unapply\" used in String matching" should {

    def testDay(day:String) = day match {
      case Day(d) => d
      case _ => null
    }

    def testFoo(x:String) = x match {
      case Foo(f) => f
      case _ => null
    }

    "match known values" in {
      testDay("TUE") === Day.TUE
      testDay("FRI") === Day.FRI
      testFoo("baz") === Foo.BAZ
    }

    "not match unknown values" in {
      testDay("Gomer Pile") === null
      testDay("Snakes on a Plane") === null
      testFoo("fiddlesticks") === null
    }

    "neither throw exceptions nor match null values" in {
      testDay(null) === null
      testFoo(null) === null
    }
  }

  "\"unapply\" used in value matching" should {

    def testDay(day:Day.Value) = day match {
      case Day(d) => d
      case _ => "fail"
    }

    def testFoo(x:Foo.Value) = x match {
      case Foo(f) => f
      case _ => "fail"
    }

    "match known values" in {
      testDay(Day.WED) === "WED"
      testDay(Day.SAT) === "SAT"
      testFoo(Foo.FOO) === "foo"
    }

    "not match null values" in {
      testDay(null) === "fail"
      testFoo(null) === "fail"
    }
  }
}

