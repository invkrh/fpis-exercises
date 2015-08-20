package me.invkrh.fpis.ch4

import org.scalatest.FunSuite
import Exercise._

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

  test("4.2: variance") {
    assertResult(Some(5.25)) {
      variance(Seq(1, 2, 3, 4, 5, 6, 7, 8))
    }

    assertResult(None) {
      variance(Seq())
    }
  }

  test("4.3: map2") {

    val optNone: Option[Int] = None

    assertResult(Some(5)) {
      map2(Some(2), Some(3))(_ + _)
    }

    assertResult(None) {
      map2(Some(3), optNone)(_ + _)
    }

    assertResult(None) {
      map2(optNone, Some(3))(_ + _)
    }
  }

  test("4.4: sequence") {

    val optA: Option[Int] = Some(1)
    val optB: Option[Int] = Some(2)
    val optC: Option[Int] = Some(3)

    assertResult(Some(List(1, 2, 3))) {
      val l = List[Option[Int]](optA, optB, optC)
      Option.sequence(l)
    }

    assertResult(None) {
      val l = List(optB, None, optC)
      Option.sequence(l)
    }
  }

  test("4.5: traverse") {
    // just check sequence test
  }

  test("4.6: Either") {
    val good: Either[String, Int] = Right(0)
    val error: Either[String, Int] = Left("error")

    assertResult(Right(1)) {
      good.map(_ + 1)
    }

    assertResult(error) {
      error.map(_ + 1)
    }

    assertResult(Right(1)) {
      good.flatMap(x => Right(x + 1))
    }

    assertResult(error) {
      good.flatMap(x => error)
    }

    assertResult(error) {
      error.flatMap(x => Right(x + 1))
    }

    assertResult(good) {
      good.orElse(error)
    }

    assertResult(good) {
      error.orElse(good)
    }

    assertResult(good) {
      good.map2(good) {
        (a, b) => a + b
      }
    }

    assertResult(error) {
      good.map2(error) {
        (a, b) => a + b
      }
    }

    assertResult(error) {
      error.map2(error) {
        (a, b) => a + b
      }
    }
  }

  test("4.7") {
    val eth0: Either[String, Int] = Right(0)
    val eth1: Either[String, Int] = Right(1)
    val eth2: Either[String, Int] = Right(2)
    val eth3: Either[String, Int] = Right(3)
    val error: Either[String, Int] = Left("error")

    assertResult(Right(List(0, 1, 2, 3))) {
      Either.sequence(List(eth0, eth1, eth2, eth3))
    }

    assertResult(error) {
      Either.sequence(List(eth0, error, eth2, eth3))
    }
  }

  test("4.8: report 2 errors") {
    assertResult(2) {
      mkPerson("", -1) match {
        case Right(p) => println(p)
        case Left(e) => e.size
      }
    }
  }
}
