package me.invkrh.fpis.ch4


/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 8/16/15
 * Time: 10:05 PM
 */

/**
 * 4.6
 * Implement versions of map , flatMap , orElse , and map2 on Either that operate on the
 * Right value.
 */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => Right(a)
      case Left(e) => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap {
      aa => b.map(bb => f(aa, bb))
    }

  /**
   * 4.8
   */
  def map2bis[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] =
    (this, b) match {
      case (Left(e1), Left(e2)) => Left(List(e1, e2))
      case (Left(e1), _) => Left(List(e1))
      case (_, Left(e2)) => Left(List(e2))
      case (Right(a1), Right(a2)) => Right(f(a1, a2))
    }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  /**
   * 4.7
   * Implement sequence and traverse for Either . These should return the first error
   * that’s encountered, if there is one.
   */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    val z: Either[E, List[B]] = Right(List[B]())
    as.foldRight(z) {
      case (elem, acc) => acc.map2(f(elem)) { (l, v) => v :: l }
    }
  }

}


