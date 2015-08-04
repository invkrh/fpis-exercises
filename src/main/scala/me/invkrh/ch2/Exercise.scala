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

  /**
   * 2.2
   * Implement isSorted, which checks whether an `Array[A]` is sorted according to a given
   * comparison function:
   */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def checkOrder(n: Int): Boolean = {
      if (n < as.length - 1) {
        if (ordered(as(n), as(n + 1)))
          checkOrder(n + 1)
        else false
      } else true
    }
    checkOrder(0)
  }

  /**
   * 2.3
   * Write the implementation of the following function
   */
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    (a: A) => (b: B) => f(a, b)
  }

  /**
   * 2.4
   * Implement `uncurry` , which reverses the transformation of `curry`.
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
   * 2.5
   * Implement the higher-order function that composes two functions
   */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}
