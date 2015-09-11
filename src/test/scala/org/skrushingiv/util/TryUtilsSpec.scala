package org.skrushingiv.util

import org.specs2.mutable._
import scala.util.{Try,Success,Failure}

class TryUtilsSpec extends Specification {

  "\"fold\" on a Success" should {
    var y = 0
    val x = Success(1337).fold(_ * 2, {_ => y = 1; 0})

    "return the result of the success function" in {
      x === 2674
    }

    "not execute the failure function" in {
      y === 0
    }
  }

  "\"fold\" on a Failure" should {
    var y = 0
    val x = Failure(new Exception("foo")).fold({_ => y = 1; "bar"}, _.getMessage)

    "return the result of the failure function" in {
      x === "foo"
    }

    "not execute the success function" in {
      y === 0
    }
  }

}

