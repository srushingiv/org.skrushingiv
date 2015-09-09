package org.skrushingiv.util

import scala.util.Try

/**
 * Reusable enumeration extractor module functions.
 * 
 * You can define your Enumeration modules as normal, and add this trait in order to
 * gain the ability to use your Module as an Extractor based on value names.
 * 
 * For example:
 * 
 *     object Foo extends Enumeration with EnumerationNameExtractor {
 *       val BAR, BAZ = Value
 *     }
 * 
 *     someString match {
 *       case Foo(b) => println("matched!")
 *       case _ => println("did not match.")
 *     }
 */
trait EnumerationNameExtractor { self : Enumeration =>

  /**
   * Return a `Value` from this `Enumeration` whose name matches
   *  the argument `name`.  See scala.Enumeration#withName(String)
   *
   * @param  s an `Enumeration` name
   * @return   the `Value` of this `Enumeration` if its name matches `s`
   * @throws   java.util.NoSuchElementException if no `Value` with a matching
   *           name is in this `Enumeration`
   */
  def apply(name:String): self.Value = withName(name)

  /**
   * Extractor for case-match comprehensions against strings.
   */
  def unapply(name:String): Option[self.Value] = Try(withName(name)).toOption

  /**
   * Extractor for case-match comprehensions against values.
   */
  def unapply(t:self.Value): Option[String] = Some(t.toString)

}