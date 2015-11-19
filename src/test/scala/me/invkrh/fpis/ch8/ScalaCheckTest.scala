package me.invkrh.fpis.ch8

import me.invkrh.fpis.ch6.SimpleRNG
import org.scalatest.FunSuite

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 10/21/15
 * Time: 11:38 PM
 */

class ScalaCheckTest extends FunSuite {

  import me.invkrh.fpis.ch8.ScalaCheck.Gen._

  val g = SimpleRNG(42L)

  test("8.4: choose") {
    assertResult(true) {
      val rd = choose(2, 5)
      val res = rd.sample.run(g)._1
      res >= 2 && res < 5
    }
  }

  test("Option: random String") {
    assertResult(true) {
      1 to 5 foreach {
        i =>
          println(rdString(20))
      }
      true
    }
  }
}
