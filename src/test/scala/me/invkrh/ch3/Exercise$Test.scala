package me.invkrh.ch3

import me.invkrh.ch3.Exercise._
import org.scalatest.FunSuite

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 8/9/15
 * Time: 11:42 PM
 */

class Exercise$Test extends FunSuite {

  test("3.1: res_3_1") {
    assertResult(3) {
      res_3_1
    }
  }

  test("3.2: tail") {
    assertResult(List(2, 3, 4)) {
      tail(List(1, 2, 3, 4))
    }
  }

  test("3.3: setHead") {
    assertResult(List(5, 2, 3, 4)) {
      setHead(List(1, 2, 3, 4), 5)
    }
  }

  test("3.4: drop") {
    assertResult(Nil) {
      drop(List(1, 2, 3, 4), 5)
    }

    assertResult(List(4)) {
      drop(List(1, 2, 3, 4), 3)
    }

    assertResult(Nil) {
      drop(List(1, 2, 3, 4), 100)
    }
  }

  test("3.5: dropWhile") {
    assertResult(Nil) {
      dropWhile(List(1, 2, 3, 4))(x => true)
    }

    assertResult(List(1, 2, 3, 4)) {
      dropWhile(List(1, 2, 3, 4))(x => false)
    }

    assertResult(List(3, 4)) {
      dropWhile(List(2, 3, 4))(_ % 2 == 0)
    }
  }

  test("3.6: init") {
    assertResult(List(1, 2, 3)) {
      init(List(1, 2, 3, 4))
    }

    assertResult(List(1)) {
      init(List(1, Nil))
    }
  }

  test("3.7: product") {

    assertResult(24) {
      List.product(List(1, 2, 3, 4))
    }

    assertResult(0) {
      List.product(List(1, 0, 3, 4))
    }
  }

  test("3.7: product, foldRight with short-circuiting") {
    // see doc
  }

  test("3.8: res_3_8") {
    assertResult(List(1, 2, 3)) {
      res_3_8
    }
  }

  test("3.9: length") {
    assertResult(3) {
      length(List(1, 2, 3))
    }
  }

  test("3.10: foldLeft") {
    assertResult(6) {
      foldLeft(List(1, 2, 3), 0)(_ + _)
    }
  }

  test("3.11: sum, product, length with foldLeft") {
    assertResult(6) {
      WithFoldLeft.sum(List(1, 2, 3))
    }

    assertResult(6) {
      WithFoldLeft.product(List(1, 2, 3))
    }

    assertResult(3) {
      WithFoldLeft.length(List(1, 2, 3))
    }
  }

  test("3.12: reverse with fold") {
    assertResult(List(3, 2, 1)) {
      reverse(List(1, 2, 3))
    }
  }

  test("3.13: foldLeft(Right) in terms of foldRight(Left)") {
    assertResult(6) {
      FoldLeft.foldLeft(List(1, 2, 3), 0)(_ + _)
    }

    assertResult(6) {
      foldRight(List(1, 2, 3), 0)(_ + _)
    }
  }

  test("3.14: append") {
    assertResult(List(1, 2, 3, 4)) {
      append(List(1, 2, 3), 4)
    }
  }

  test("3.15: concat") {
    assertResult(List(1, 2, 3, 4, 5)) {
      concat(List(List(1, 2), List(3, 4), List(5)))
    }
  }

  test("3.16: increment") {
    assertResult(List(2, 3, 4, 5, 6)) {
      increment(List(1, 2, 3, 4, 5))
    }
  }

  test("3.17: double2String") {
    assertResult(List("1.0", "2.0", "3.0", "4.0", "5.0")) {
      double2String(List(1d, 2d, 3d, 4d, 5d))
    }
  }

  test("3.18: map") {
    assertResult(List("1.0", "2.0", "3.0", "4.0", "5.0")) {
      map(List(1d, 2d, 3d, 4d, 5d))(_.toString)
    }
  }

  test("3.19: filter") {
    assertResult(List(2, 4)) {
      filter(List(1, 2, 3, 4, 5))(_ % 2 == 0)
    }
  }

  test("3.20: flatMap") {
    assertResult(List(1, 1, 2, 2, 3, 3)) {
      flatMap(List(1, 2, 3))(i => List(i, i))
    }
  }

  test("3.21: filter with flatMap") {
    assertResult(List(2, 4)) {
      WithFlatMap.filter(List(1, 2, 3, 4, 5))(_ % 2 == 0)
    }
  }

  test("3.22: zipAdd") {
    assertResult(List(5, 7, 9)) {
      zipAdd(List(1, 2, 3), List(4, 5, 6))
    }
  }

  test("3.23: zipWith") {
    assertResult(List(5, 7, 9)) {
      zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _)
    }
  }

  test("3.24: hasSubsequence") {
    assertResult(true) {
      hasSubsequence(List(1, 2, 3, 4), List(1, 2))
    }

    assertResult(false) {
      hasSubsequence(List(1, 2, 3, 4), List(1, 3))
    }
  }

  val testTree: Tree[Int] =
    Branch(
      Branch(Leaf(1), Leaf(2)),
      Branch(
        Branch(Leaf(3), Leaf(4)),
        Leaf(5))
    )

  test("3.25: size") {
    assertResult(5) {
      size(testTree)
    }
  }

  test("3.26: maximun") {
    assertResult(5) {
      maximum(testTree)
    }
  }

  test("3.27: depth") {
    assertResult(3) {
      depth(testTree)
    }
  }

  test("3.28: map") {
    assertResult(
      Branch(
        Branch(Leaf(2), Leaf(3)),
        Branch(
          Branch(Leaf(4), Leaf(5)),
          Leaf(6))
      )
    ) {
      map(testTree)(_ + 1)
    }
  }

  test("3.29: tree fold") {
    assertResult(5) {
      WithTreeFold.size(testTree)
    }

    assertResult(5) {
      WithTreeFold.maximum(testTree)
    }

    assertResult(3) {
      WithTreeFold.depth(testTree)
    }

    assertResult(
      Branch(
        Branch(Leaf(2), Leaf(3)),
        Branch(
          Branch(Leaf(4), Leaf(5)),
          Leaf(6))
      )
    ) {
      WithTreeFold.map(testTree)(_ + 1)
    }
  }
}