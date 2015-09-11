package org.skrushingiv.util

import org.specs2.mutable._

class AndSpec extends Specification {

  "Calling \"and\" with a function argument" should {

    val x = 1
    var y = 3

    def increment(t:Int) = y = y + t

    val z = x and increment

    "return the wrapped value" in {
      z === 1
    }

    "apply the side effect function" in {
      y === 4
    }
  }

  "Calling \"and\" with an inline function argument" should {

    val x = 5
    var y = 9

    val z = x and (y += _)

    "return the wrapped value" in {
      z === 5
    }

    "apply the side effect function" in {
      y === 14
    }
  }

}

