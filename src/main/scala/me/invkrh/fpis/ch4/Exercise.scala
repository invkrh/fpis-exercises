package me.invkrh.fpis.ch4

import me.invkrh.fpis.ch3.List

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 8/12/15
 * Time: 10:47 PM
 */

object Exercise {

  /**
   * 4.1
   * See Option.scala in this package
   */

  /**
   * 4.2
   * Implement the variance function in terms of flatMap.
   * If the mean of a sequence is `m`, the variance is the mean of `math.pow(x - m, 2)` for each
   * element `x` in the sequence.
   */
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]) = if (xs.isEmpty) None else Some(xs.sum / xs.length)
    mean(xs) flatMap {
      m => mean(xs.map(x => math.pow(x - m, 2)))
    }
  }

  /**
   * lift:
   * the map function lets us operate on values of type Option[A] using a
   * function of type A => B , returning Option[B] . Another way of looking at this is that map
   * turns a function f of type A => B into a function of type Option[A] => Option[B]
   */
  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  /**
   * 4.3
   * Write a generic function map2 that combines two Option values using a binary
   * function. If either Option value is None , then the return value is too
   */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap {
      aa => b.map(bb => f(aa, bb))
    }

  /**
   * 4.4
   * See Option.scala in this package
   */

  /**
   * 4.5
   * See Option.scala in this package
   */

  /**
   * 4.8
   * See `map2bis` in Either.scala
   */

  case class Person(name: Name, age: Age)
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[List[String], Person] =
    mkName(name).map2bis(mkAge(age))(Person.apply)

}
