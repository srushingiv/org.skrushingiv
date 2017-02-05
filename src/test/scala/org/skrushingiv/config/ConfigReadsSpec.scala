package org.skrushingiv.config

import com.typesafe.config._

import org.specs2.mutable._
import play.api.libs.json._

object ConfigReadsSpec {

  case class Alpha(i: Int, d: Double, s: String, t: Boolean, f: Boolean, n: Option[String])

  implicit val alphaReads = new ConfigReads[Alpha] {
    def read(value: ConfigValue): Option[Alpha] = {
      for {
        i ← value.get("integer").as[Int]
        d ← value.get("double").as[Double]
        s ← value.get("string").as[String]
        t ← value.get("booleanT").as[Boolean]
        f ← value.get("booleanF").as[Boolean]
        n = value.get("nothing").as[String]
      } yield Alpha(i, d, s, t, f, n)
    }
  }

}

class ConfigReadsSpec extends Specification {
  import ConfigReadsSpec._

  val config = ConfigFactory.parseString("""
    |test {
    |  alpha {
    |    integer = 123
    |    double = 12.34
    |    string = something cool
    |    booleanT = true
    |    booleanF = false
    |    nothing = null
    |  }
    |  strings = [
    |    lorem ipsum
    |    dolor amet
    |  ]
    |}
    |""".stripMargin)

  "ConfigReads" should {
    "be able to parse an Integer" in {
      config.get("test.alpha.integer").as[Int] === Some(123)
    }
    "be able to parse a Long" in {
      config.get("test.alpha.integer").as[Long] === Some(123L)
    }
    "be able to parse a Double" in {
      config.get("test.alpha.double").as[Double] === Some(12.34)
    }
    "be able to parse a String" in {
      config.get("test.alpha.string").as[String] === Some("something cool")
    }
    "be able to parse a Boolean" in {
      config.get("test.alpha.booleanT").as[Boolean] === Some(true)
      config.get("test.alpha.booleanF").as[Boolean] === Some(false)
    }
    "be able to parse a null" in {
      config.get("test.alpha.nothing").as[Null] === Some(null)
    }
    "return None when the path is missing" in {
        config.get("test.beta").as[ConfigValue] === None
    }
    "return None when the value type doesn't match" in {
      config.get("test.alpha.integer").as[String] === None
      config.get("test.alpha.string").as[Long] === None
    }
    "return Null values for inspection" in {
      val v = config.get("test.alpha.nothing")
      v.isDefined === true
      v.as[Null] === Some(null)
    }
    "be able to parse a arbitrary complex object when supplied a valid reader" in {
      config.get("test.alpha").as[Alpha] === Some(Alpha(123, 12.34, "something cool", true, false, None))
    }
    "be able to parse a list" in {
      config.get("test.strings").as[List[String]] === Some(List("lorem ipsum", "dolor amet"))
    }
  }
}
