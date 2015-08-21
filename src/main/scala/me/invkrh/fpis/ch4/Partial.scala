package me.invkrh.fpis.ch4

/**
 * Created with IntelliJ IDEA.
 * User: Hao
 * Date: 8/21/15
 * Time: 1:23 PM
 */

trait Partial[+A, +B] {
  def map[C](f: B => C): Partial[A, C] = {
    this match {
      case Success(b) => Success(f(b))
      case Errors(as) => Errors(as)
    }
  }

  def flatMap[AA >: A, C](f: B => Partial[AA, C]): Partial[AA, C] =
    this match {
      case Success(b) => f(b)
      case Errors(as) => Errors(as)
    }

  def map2[AA >: A, C, D](b: Partial[AA, C])(f: (B, C) => D): Partial[AA, D] =
    (this, b) match {
      case (Errors(es1), Errors(es2)) => Errors(es1 ++ es2)
      case (Errors(e1), _) => Errors(e1)
      case (_, Errors(e2)) => Errors(e2)
      case (Success(a1), Success(a2)) => Success(f(a1, a2))
    }
}

case class Errors[+A](get: Seq[A]) extends Partial[A, Nothing]

case class Success[+B](get: B) extends Partial[Nothing, B]

object Partial {

  def sequence[A, B](es: List[Partial[A, B]]): Partial[A, List[B]] =
    traverse(es)(x => x)

  def traverse[A, B, C](as: List[B])(f: B => Partial[A, C]): Partial[A, List[C]] = {
    val z: Partial[A, List[C]] = Success(List[C]())
    as.foldRight(z) {
      case (elem, acc) => f(elem).map2(acc) { (v, l) => v :: l }
    }
  }
}


