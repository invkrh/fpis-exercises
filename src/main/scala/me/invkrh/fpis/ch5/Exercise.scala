package me.invkrh.fpis.ch5

import Stream._

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 8/31/15
 * Time: 9:40 PM
 */

object Exercise {

  /**
   * Shadowed by `ones` via `unfold`
   */
  //  def ones: Stream[Int] = Stream.cons(1, ones)

  /**
   * 5.8
   *
   * Generalize ones slightly to the function constant , which returns an infinite `Stream` of
   * a given value.
   */
  /**
   * Shadowed by `constant` via `unfold`
   */
  //  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  // This is more efficient than `cons(a, constant(a))` since it's just
  // one object referencing itself.

  //  def constant[A](a: A): Stream[A] = {
  //    lazy val tail: Stream[A] = Cons(() => a, () => tail)
  //    tail
  //  }

  /**
   * 5.9
   *
   * Write a function that generates an infinite stream of integers, starting from n,
   * then n + 1 , n + 2 , and so on.
   */
  /**
   * Shadowed by `from` via `unfold`
   */
  //  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  /**
   * 5.10
   *
   * Write a function fibs that generates the infinite stream of Fibonacci numbers:
   * 0, 1, 1, 2, 3, 5, 8, and so on.
   */
  /**
   * Shadowed by `fibs` via `unfold`
   */
  //  def fibs() = {
  //    def rec(current: Int, next: Int): Stream[Int] =
  //      Stream.cons(current, rec(next, current + next))
  //    rec(0, 1)
  //  }

  /**
   * 5.12
   *
   * Write fibs, from, constant, and ones in terms of unfold.
   */
  def fibs(): Stream[Int] =
    unfold((0, 1)) {
      case (current, next) => Some((current, (next, current + next)))
    }

  def from(n: Int): Stream[Int] =
    unfold(n) {
      n => Some(n, n + 1)
    }

  def constant[A](a: A): Stream[A] =
    unfold(a) {
      a => Some(a, a)
    }

  def ones(): Stream[Int] =
    unfold(1) {
      _ => Some(1, 1)
    }

}
