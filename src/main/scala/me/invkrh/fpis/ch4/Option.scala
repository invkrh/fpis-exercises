package me.invkrh.fpis.ch4

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 8/15/15
 * Time: 10:56 AM
 */

/** 4.1
  * Implement all of the preceding functions on Option . As you implement each function,
  * try to think about what it means and in what situations you’d use it. We’ll explore when
  * to use each of these functions next. Here are a few hints for solving this exercise:
  * - It’s fine to use pattern matching, though you should be able to implement all
  * the functions besides map and getOrElse without resorting to pattern matching.
  * - For map and flatMap , the type signature should be enough to determine the implementation.
  * - getOrElse returns the result inside the Some case of the Option , or if the Option
  * is None , returns the given default value.
  * - orElse returns the first Option if it’s defined; otherwise, it returns the second Option .
  */

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case Some(x) => Some(f(x))
      case None => None
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case Some(x) => f(x)
      case None => None
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(x) => x
      case None => default
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case Some(x) => this
      case None => ob
    }

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(x) if f(x) => Some(x)
      case _ => None
    }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


