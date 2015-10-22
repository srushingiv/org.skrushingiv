package org.skrushingiv.test

import org.specs2.mutable.Specification
import org.specs2.execute.AsResult

trait ConditionalSkipping { self : Specification =>

  /**
   * Skips execution of unit tests if some predicate fails.
   */
  def skipIf[R](shouldSkip:Boolean)(body: => R)(implicit evidence: AsResult[R]) =
    if (shouldSkip) skipped else evidence.asResult(body)

}