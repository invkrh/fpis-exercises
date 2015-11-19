package me.invkrh.fpis.ch6

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 9/6/15
 * Time: 11:02 PM
 */

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long = System.currentTimeMillis()) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
