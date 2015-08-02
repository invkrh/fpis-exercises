package me.invkrh.ch2

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 03/08/15
 * Time: 00:31
 */

object Exercise {

  /**
   * 2.1
   * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
   * The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the
   * previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a
   * local tail-recursive function.
   */
  def fib(n: Int): Int = {
    @tailrec
    def fibRec(n: Int, accSmallOne: Int, accLargeOne: Int): Int = {
      if (n == 0) accSmallOne
      else if (n == 1) accLargeOne
      else fibRec(n - 1, accLargeOne, accSmallOne + accLargeOne)
    }
    fibRec(n, 0, 1)
  }

}
