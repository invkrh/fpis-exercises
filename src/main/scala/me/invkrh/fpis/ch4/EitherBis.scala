package me.invkrh.fpis.ch4

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 8/18/15
 * Time: 11:32 PM
 */

import me.invkrh.fpis.ch3.List

trait EitherBis[+E, +A] {
  def map[B](f: A => B): EitherBis[E, B] =
    this match {
      case RightBis(a) => RightBis(f(a))
      case LeftBis(es) => LeftBis(es)
    }
}

case class LeftBis[+E](value: List[E]) extends EitherBis[E, Nothing]
case class RightBis[+A](value: A) extends EitherBis[Nothing, A]
