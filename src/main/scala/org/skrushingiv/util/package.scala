package org.skrushingiv

import scala.util.{Try, Success, Failure}

package object util {

  implicit class And[T](val self:T) extends AnyVal {
    /**
     * Allows for a side-effecting function for syntatic sugar.
     * 
     * So, instead of having to write:
     *     val a = foo()
     *     bar(a)
     *     a
     * 
     * You can, instead, write:
     *     foo() and (bar(_))
     */
    def and(effect:T => Unit): T = { effect(self); self }
  }


  implicit class IterableUtils[T <: Iterable[_]](val self:T) extends AnyVal {

    /**
     * Optionally applies a transformation on this collection if it is not empty.
     */
    def ifDefined[A](xform:T => A): Option[A] = if (!self.isEmpty) Some(xform(self)) else None

    /**
     * Replaces this collection with an Option containing the `value` if and only if
     * this collection is empty.
     */
    def ifEmpty[A](value: => A): Option[A] = if (self.isEmpty) Some(value) else None

    /**
     * Executes a side-effect if and only if this collection is empty.
     */
    def onEmpty(doEffect: => Unit): T = self and (_ ifEmpty doEffect)
  }


  implicit class TryUtils[A](val self: Try[A]) extends AnyVal {

    /**
     * Similar to the standard `Try.transform(s,f)`, this method completes this
     * `Try` by applying the function `f` to this if this is of type `Failure`, or
     * conversely, by applying `s` if this is a `Success`.
     */
    def fold[B](s: A => B, f: Throwable => B) : B = self match {
      case Failure(t) => f(t)
      case Success(a) => s(a)
    }

  }

}
