package me.invkrh.fpis.ch2

import Exercise._
import org.scalatest.{FunSuite, FlatSpec}

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 03/08/15
 * Time: 01:26
 */

class Exercise$Test extends FunSuite {

  test("2.1: fib") {
    assertResult(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)) {
      0 to 9 map fib toList
    }
  }

  test("2.2: isSorted") {
    assertResult(true) {
      isSorted[Int](Array(1, 2, 3, 4, 5), _ < _)
    }

    assertResult(false) {
      isSorted[Int](Array(1, 2, 3, 4, 5), _ > _)
    }

    assertResult(false) {
      isSorted[Int](Array(1, 6, 3, 4, 5), _ > _)
    }
  }
}
