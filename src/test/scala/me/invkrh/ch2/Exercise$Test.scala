package me.invkrh.ch2

import Exercise._
import org.scalatest.FlatSpec

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 03/08/15
 * Time: 01:26
 */

class Exercise$Test extends FlatSpec {

  "fib" should "return a correct Fibonacci number based on n" in {
    assertResult(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)) {
      0 to 9 map fib toList
    }
  }

}
