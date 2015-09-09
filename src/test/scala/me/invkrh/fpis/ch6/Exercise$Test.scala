package me.invkrh.fpis.ch6

import org.scalatest.FunSuite
import me.invkrh.fpis.ch6.Exercise._

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 9/6/15
 * Time: 11:07 PM
 */

class Exercise$Test extends FunSuite {

  val g = SimpleRNG(42L)

  test("6.1: nonNegativeInt") {
    assertResult(16159453) {
      nonNegativeInt(g)._1
    }
  }

  test("6.2: double") {
    assertResult(0.007524831686168909) {
      double(g)._1
    }
  }

  test("6.3: intDouble, doubleInt, double3") {

    assertResult((16159453, 0.5967354848980904)) {
      intDouble(g)._1
    }

    assertResult((0.007524831686168909, -1281479697)) {
      doubleInt(g)._1
    }

    assertResult((0.007524831686168909, 0.5967354848980904, 0.15846728393808007)) {
      double3(g)._1
    }
  }

  test("6.4: ints") {
    assertResult(List(-340305902, -1281479697, 16159453)) {
      ints(3)(g)._1
    }
  }

  test("6.5: double via map") {
    assertResult(0.007524831686168909) {
      doubleViaMap(g)._1
    }
  }

  test("6.6: map2") {
    assertResult((16159453, 0.5967354848980904)) {
      randIntDouble(g)._1
    }
    assertResult((0.007524831686168909, -1281479697)) {
      randDoubleInt(g)._1
    }
  }

  test("6.7: sequence") {
    assertResult(List(16159453, 1281479696)) {
      sequence(List(int, nonNegativeEven))(g)._1
    }
  }

  test("6.8: flatMap") {
    // see 6.9
  }

  test("6.9: mapViaFlatMap, map2ViaFlatMap") {
    assertResult(0.007524831686168909) {
      mapViaFlatMap(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(g)._1
    }

    assertResult((16159453, 0.5967354848980904)) {
      map2ViaFlatMap(int, doubleViaMap)((_, _))(g)._1
    }
  }

  test("6.10: State") {
    // Retest RNG using:
    type Rand[A] = State[RNG, A]
  }

  test("6.11: Machine") {
    assertResult((14, 1)) {
      val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
      Machine.simulateMachine(inputs).run(Machine(locked = true, 5, 10))._1
    }
  }

}
