package org.skrushingiv.util

import org.specs2.mutable._
import org.specs2.specification.Scope

class InitializerSpec extends Specification {

  trait Fixture extends Scope {
    var x:Option[String] = None
    var y:Option[(String, Any)] = None
    val TestInitializer = Initializer(
      before = (name:String) => { x = Some(name) },
      after = (name:String, value:Any) => { y = Some(name -> value) }
    )
  }

  "initializing with sugar" should {

    "execute the default side-effects when no implicits are available" in new Fixture {
      import TestInitializer._
      val z = "abc" is 123
      
      x === Some("abc")
      y === Some("abc" -> 123)
      z === 123
    }

    "execute the implicit side effects when available" in new Fixture {
      import TestInitializer._
      implicit def afterIntInitializer(name:String,value:Int) = {
        y = Some( name -> ("test",value))
      }
      
      val z1 = "abc" is 123
      lazy val z2 = "def" is "456"
      
      x === Some("abc")
      y === Some("abc" -> ("test",123))
      z1 === 123
      z2 === "456"
      x === Some("def")
      y === Some("def" -> "456")
    }
  }

  "initializing without sugar" should {

    "execute the default side-effects when no implicits are available" in new Fixture {
      val z = TestInitializer("abc") { 123 }
      
      x === Some("abc")
      y === Some("abc" -> 123)
      z === 123
    }

    "execute the implicit side effects when available" in new Fixture {
      implicit def afterIntInitializer(name:String,value:String) = {
        y = Some( name -> ("test",value))
      }
      
      val z1 = TestInitializer("abc") { 123 }
      lazy val z2 = TestInitializer("def") { "456" }
      
      x === Some("abc")
      y === Some("abc" -> 123)
      z1 === 123
      z2 === "456"
      x === Some("def")
      y === Some("def" -> ("test","456"))
    }
  }

}

