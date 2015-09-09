package me.invkrh.fpis.ch6

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 9/6/15
 * Time: 11:00 PM
 */

object Exercise {

  /**
   * 6.1
   *
   * Write a function that uses RNG.nextInt to generate a random integer between 0 and
   * `Int.maxValue` (inclusive). Make sure to handle the corner case when nextInt returns
   * `Int.MinValue`, which does not have a non-negative counterpart.
   */

  /*
   * Signed integer negation:
   * - Invert all the bits through the number
   * - Add one
   *
   * Hence,
   * Int.maxValue = 2 ^ 31 - 1 = Int.mimValue - 1
   * Int.mimValue = - 2 ^ 31 = - Int.mimValue
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, g) = rng.nextInt
    (if (v < 0) -(v + 1) else v, g)
  }

  /**
   * 6.2
   *
   * Write a function to generate a Double between 0 and 1, not including 1.
   * Note: You can use `Int.MaxValue` to obtain the maximum positive integer value,
   * and you can use `x.toDouble` to convert an `x: Int` to a `Double`.
   */
  def double(rng: RNG): (Double, RNG) = {
    val (v, g) = nonNegativeInt(rng)
    (v / (Int.MaxValue.toDouble + 1), g)
  }

  /**
   * 6.3
   *
   * Write functions to generate an `(Int, Double)` pair, a `(Double, Int)` pair, and a
   * `(Double, Double, Double)` 3-tuple. You should be able to reuse the functions you’ve
   * already written.
   */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, g1) = rng.nextInt
    val (d, g2) = double(g1)
    ((i, d), g2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, g1) = double(rng)
    val (i, g2) = g1.nextInt
    ((d, i), g2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, g1) = double(rng)
    val (d2, g2) = double(g1)
    val (d3, g3) = double(g2)
    ((d1, d2, d3), g3)
  }

  /**
   * 6.4
   *
   * Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def rec(n: Int, acc: List[Int])(rng: RNG): (List[Int], RNG) = {
      if (n == 0) (acc, rng)
      else {
        val (v, g) = rng.nextInt
        println("v1 = " + v)
        rec(n - 1, v :: acc)(g)
      }
    }
    rec(count, Nil)(rng)

    /** `reverse` is optional, since it is random */
    //    val (l, g) = rec(count, Nil)(rng)
    //    (l.reverse, g)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /**
   * 6.5
   *
   * Use map to reimplement double in a more elegant way. See exercise 6.2.
   */
  val doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /**
   * 6.6
   *
   * Write the implementation of map2 based on the following signature. This function
   * takes two actions, ra and rb, and a function f for combining their results, and returns
   * a new action that combines them.
   */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    r => {
      val (va, ga) = ra(r)
      val (vb, gb) = rb(ga)
      (f(va, vb), gb)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, doubleViaMap)
  val randDoubleInt: Rand[(Double, Int)] =
    both(doubleViaMap, int)

  /**
   * 6.7
   *
   * Hard: If you can combine two RNG transitions, you should be able to combine a whole
   * list of them. Implement sequence for combining a `List` of transitions into a single
   * transition. Use it to reimplement the ints function you wrote before. For the latter,
   * you can use the standard library function `List.fill(n)(x)` to make a list with x
   * repeated n times.
   */

  /**
   * Need to mention that `sequence` via `foldLeft` has the same result as ints, and the
   * generators are used in the right order. However, both of them need be reversed to
   * make result correctly ordered.
   */
  //  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  //    r => fs.foldLeft((List[A](), r)) {
  //      case ((l, rng), rand) =>
  //        val (v, g) = rand(rng)
  //        (v :: l, g)
  //    }

  /**
   * An other approach is to use `foldRight` to get a correctly ordered result.
   * However, the problem is the seed is first used to generate the last `Rand[A]`
   * in the list.
   */
  //  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  //    r => fs.foldRight((List[A](), r)) {
  //      case (rand, (l, rng)) =>
  //        val (v, g) = rand(rng)
  //        println("v2 = " + v)
  //        (v :: l, g)
  //    }

  /**
   * The correct one is the following:
   */
  // In `sequence`, the base case of the fold is a `unit` action that returns
  // the empty list. At each step in the fold, we accumulate in `acc`
  // and `f` is the current element in the list.
  // `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
  // We map over that to prepend (cons) the element onto the accumulated list.
  //
  // We are using `foldRight`. If we used `foldLeft` then the values in the
  // resulting list would appear in reverse order. It would be arguably better
  // to use `foldLeft` followed by `reverse`. What do you think?
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def _ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  /**
   * 6.8
   *
   * Implement `flatMap`, and then use it to implement `nonNegativeLessThan`.
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rnga) = f(rng)
      g(a)(rnga)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0)
          unit(mod)
        else nonNegativeLessThan(n)
    }

  /**
   * 6.9
   *
   * Reimplement `map` and `map2` in terms of `flatMap`. The fact that this is possible is what
   * we’re referring to when we say that `flatMap` is more powerful than map and map2.
   */
  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(x => unit(f(x)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a, b)))

}
