package me.invkrh.fpis.ch4

import me.invkrh.fpis.ch4.Exercise._

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
    map(f) getOrElse None

  //    this match {
  //      case Some(x) => f(x)
  //      case None => None
  //    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(x) => x;
      case None => default;
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  //    this match {
  //      case None => ob
  //      case _ => this
  //    }

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  //    this match {
  //      case Some(x) if f(x) => Some(x)
  //      case _ => None
  //    }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  /**
   * 4.4
   * Write a function sequence that combines a list of Option s into one Option containing
   * a list of all the Some values in the original list.
   * If the original list contains None even once, the result of the function should be None;
   * otherwise the result should be Some with a list of all the values.
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }

  /**
   * 4.5
   * Implement this function. It’s straightforward to do using `map` and `sequence`, but try
   * for a more efficient implementation that only looks at the list once.
   * In fact, implement `sequence` in terms of `traverse`.
   */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    val z: Option[List[B]] = Some(List[B]())
    a.foldRight(z) {
      case (elem, acc) => map2(acc, f(elem)) {
        case (l, v) => v :: l
      }
    }
  }
}


