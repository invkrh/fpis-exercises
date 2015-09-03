package me.invkrh.fpis.ch5

import org.scalatest.FunSuite
import Stream._
import me.invkrh.fpis.ch5.Exercise._


/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 8/31/15
 * Time: 9:42 PM
 */

class Exercise$Test extends FunSuite {
  test("5.1: toList") {
    assertResult(List(1, 2, 3)) {
      Stream(1, 2, 3).toList
    }

    assertResult(Nil) {
      Stream().toList
    }
  }

  test("5.2: take, drop") {
    assertResult(List(1, 2)) {
      Stream(1, 2, 3).take(2).toList
    }

    assertResult(List(1, 2)) {
      Stream(1, 2).take(3).toList
    }

    assertResult(List(3)) {
      Stream(1, 2, 3).drop(2).toList
    }
  }

  test("5.3: takeWhile") {
    assertResult(Nil) {
      Stream(1, 2, 3).takeWhile(_ % 2 == 0).toList
    }

    assertResult(List(1)) {
      Stream(1, 2).takeWhile(_ % 2 == 1).toList
    }
  }

  test("5.4: forAll") {
    assertResult(false) {
      Stream(1, 2, 3).forAll(_ % 2 == 0)
    }

    assertResult(true) {
      Stream(2, 4, 6).forAll(_ % 2 == 0)
    }
  }

  test("5.5: takeWhile via foldRight") {
    /**
     * see test 5.3
     */
  }

  test("5.6: headOption via foldRight") {
    assertResult(Some(1)) {
      Stream(1, 2, 3).headOption
    }

    assertResult(None) {
      Stream().headOption
    }
  }

  test("5.7: map, filter, append, and flatMap") {

    assertResult(List(2, 4, 6)) {
      Stream(1, 2, 3).map(_ * 2).toList
    }

    assertResult(Nil) {
      Stream[Int]().map(_ * 2).toList
    }

    assertResult(List(2)) {
      Stream(1, 2, 3).filter(_ % 2 == 0).toList
    }

    assertResult(List(1, 2, 3, 1, 2, 3)) {
      Stream(1, 2, 3).append(Stream(1, 2, 3)).toList
    }

    assertResult(List(1, 2, 2, 3, 3, 4)) {
      Stream(1, 2, 3).flatMap(x => Stream(x, x + 1)).toList
    }

    assertResult(List(12, 14)) {
      Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
    }
  }

  test("Observation1") {
    def v1() = {
      println("v1")
      1
    }

    def v2() = {
      println("v2")
      2
    }

    /**
     * `Stream.apply` 's arguments are strict
     * `v1()` and `v2()` will be evaluated first
     */
    Stream(v1(), v2())
    cons(v1(), cons(v2(), Empty))
  }

  test("Observation2") {
    ones().take(5).toList
    ones().exists(_ % 2 != 0)
    ones().map(_ + 1).exists(_ % 2 == 0)
    ones().takeWhile(_ == 1)
    ones().forAll(_ != 1)
    // ones.forAll(_ == 1) // stack overflow error
  }

  test("5.8: constant") {
    assertResult(List(List(1), List(1), List(1))) {
      constant(List(1)).take(3).toList
    }
  }

  test("5.9: from") {
    assertResult(List(3, 4, 5)) {
      from(3).take(3).toList
    }
  }

  test("5.10: fibs") {
    assertResult(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)) {
      fibs().take(10).toList
    }
  }

  test("5.11: unfold") {
    // see 5.12
  }

  test("5.12: fibs, from, constant, and ones") {
    // fibs see 5.10
    // from see 5.09
    // constant see 5.08
    assertResult(List(1, 1, 1)) {
      ones().take(3).toList
    }
  }

  test("5.13: map, take, takeWhile, zipWith and zipAll") {
    // map see 5.7
    // take see 5.2
    // takeWhile see 5.3
    assertResult(List(5, 7, 9)) {
      Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ + _).toList
    }

    assertResult(List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), None))) {
      val s1 = Stream(1, 2, 3)
      val s2 = Stream(1, 2)
      (s1 zipAll s2).toList
    }
  }

  test("5.14: startWith") {
    assertResult(true) {
      ones().startsWith(Stream(1, 1, 1, 1))
    }

    assertResult(false) {
      ones().startsWith(Stream(1, 2, 1, 1))
    }
  }

  test("5.15: tails & hasSubsequence") {
    assertResult(List(List(1, 2), List(2), Nil)) {
      Stream(1, 2).tails().map(_.toList).toList
    }

    assertResult(true) {
      Stream(1, 2, 3, 4).hasSubsequence(Stream(2, 3))
    }

    assertResult(false) {
      Stream(1, 2, 3, 4).hasSubsequence(Stream(1, 3))
    }
  }

  test("5.16: scanRight") {
    assertResult(List(6, 5, 3, 0)) {
      Stream(1, 2, 3).scanRight(0)(_ + _).toList
    }
  }

}
