package org.skrushingiv.util

/**
 * The Initializer class is designed around the desire to log when a variable is initialized.
 * 
 * The general use case would be something along the lines of:
 * 
 *     val myInitializer = Initializer(
 *       before = (name:String) => log(s"initializing $name),
 *       after = (name:String, value:Any) => log(s"initialized $name as $value")
 *     )
 *     import myInitializer._
 * 
 *     lazy val "foo service" is new FooService(...)
 * 
 * And when a specific post-initialization side-effect is required that handler function can be
 * defined as an implicit.
 * 
 *     implicit def postBar(name:String, value:Bar) = {
 *       val someCalc = doSomething(value)
 *       log(s"initialized $name as Bar with $someCalc")
 *     }
 *     
 *     lazy val "bar" is new Bar(...)
 * 
 */

class Initializer(before:Initializer.BeforeInitializer, defaultAfter:Initializer.AfterInitializer[Any]) {
  import Initializer._

  implicit class InitializerSugar(name:String) {
    def is[A](value: => A)(implicit initialized: AfterInitializer[A] = defaultAfter) : A = {
      before(name)
      value and (initialized(name,_))
    }
  }
}

object Initializer {
  type BeforeInitializer = String => Unit
  type AfterInitializer[A] = (String,A) => Unit

  def apply(before: BeforeInitializer = (String) => (), after: AfterInitializer[Any] = (String,Any) => () ) =
    new Initializer(before, after)
}
