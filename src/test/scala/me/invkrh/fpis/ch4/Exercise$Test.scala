package me.invkrh.fpis.ch4

import org.scalatest.FunSuite

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 8/15/15
 * Time: 11:12 AM
 */

class Exercise$Test extends FunSuite {

  test("4.1: Option functions ") {

    val optSome: Option[Int] = Some(2)
    val optNone: Option[Int] = None

    assertResult(Some(3)) {
      optSome.map(_ + 1)
    }

    assertResult(None) {
      optNone.map(_ + 1)
    }

    assertResult(Some(3)) {
      optSome.flatMap(x => Some(x + 1))
    }

    assertResult(None) {
      optSome.flatMap(x => None)
    }

    assertResult(None) {
      optNone.flatMap(x => Some(x + 1))
    }

    assertResult(2) {
      optSome.getOrElse(5)
    }

    assertResult(5) {
      optNone.getOrElse(5)
    }

    assertResult(optSome) {
      optSome.orElse(Some(5))
    }

    assertResult(Some(5)) {
      optNone.orElse(Some(5))
    }

    assertResult(optSome) {
      optSome.filter(_ % 2 == 0)
    }

    assertResult(None) {
      optSome.filter(_ % 2 != 0)
    }

    assertResult(None) {
      optNone.filter(_ % 2 == 0)
    }


  }


}
