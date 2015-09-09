package me.invkrh.fpis.ch5

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 8/31/15
 * Time: 9:25 PM
 */

trait Stream[+A] {

  import Stream._

  /**
   * Shadowed by `headOption` via `foldRight`
   */
  //  def headOption: Option[A] = this match {
  //    case Empty => None
  //    case Cons(h, t) => Some(h())
  //  }

  /**
   * 5.1
   *
   * Write a function to convert a Stream to a List , which will force its evaluation
   * and let you look at it in the REPL .
   */
  /** The nature recursive version is not stack safe */
  //  def toList: List[A] = this match {
  //    case Empty => Nil
  //    case Cons(h, t) => h() :: t().toList
  //  }

  /*
  The above solution will stack overflow for large streams, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the stream. Then at the end we reverse the result to get the
  correct order again.
  [:ben] are the line breaks above okay? I'm unclear on whether these "hints" are supposed to go in
   the book or not
  */
  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  /*
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
  */
  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  /**
   * 5.2
   *
   * Write the function take(n) for returning the first n elements of a Stream,
   * and drop(n) for skipping the first n elements of a Stream .
   */
  /**
   * Shadowed by `map` via `unfold`
   */
  //  def take(n: Int): Stream[A] = this match {
  //    case Cons(h, t) if n > 0 => Cons(h, t().take(n - 1))
  //    case _ => Empty
  //  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    // case other: Stream[A] => other
    case _ => this // better way
  }

  /**
   * 5.3
   *
   * Write the function `takeWhile` for returning all starting elements of a Stream that
   * match the given predicate.
   */

  /**
   * Shadowed by `takeWhile` via `foldRight`
   */
  //  def takeWhile(p: A => Boolean): Stream[A] = this match {
  //    case Cons(h, t) if p(h()) => Cons(h, t().takeWhile(p))
  //    case _ => Empty
  //  }

  //    def exists(p: A => Boolean): Boolean = this match {
  //      case Cons(h, t) => p(h()) || t().exists(p)
  //      case _ => false
  //    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /**
   * 5.4
   *
   * Implement forAll, which checks that all elements in the Stream match a given predicate.
   * Your implementation should terminate the traversal as soon as it encounters a non matching
   * value.
   */
  //  stack safe version
  //  @tailrec
  //  final def forAll(p: A => Boolean): Boolean = this match {
  //    case Empty => true
  //    case Cons(h, t) => p(h()) && t().forAll(p)
  //  }
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /**
   * 5.5
   *
   * Use foldRight to implement takeWhile.
   */
  /**
   * Shadowed by `map` via `unfold`
   */
  //    def takeWhile(p: A => Boolean): Stream[A] =
  //      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  /**
   * 5.6
   *
   * Hard: Implement headOption using foldRight.
   */
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  /**
   * 5.7
   *
   * Implement map, filter, append, and flatMap using foldRight.
   * The append method should be non-strict in its argument.
   */
  /**
   * Shadowed by `map` via `unfold`
   */
  //  def map[B](f: A => B): Stream[B] =
  //    foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](that: => Stream[B]): Stream[B] =
    foldRight(that)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]) =
    foldRight(empty[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  /**
   * 5.13
   *
   * Use unfold to implement `map`, `take`, `takeWhile`, `zipWith` (as in chapter 3), and
   * `zipAll`. The zipAll function should continue the traversal as long as either stream
   * has more elements—it uses Option to indicate whether each stream has been exhausted.
   */
  def map[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def take(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), cnt) if cnt > 0 => Some(h(), (t(), cnt - 1))
      case _ => None
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  // special case of `zip`
  def zip[B](s2: Stream[B]): Stream[(A, B)] = zipWith(s2)((_, _))

  //  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
  //    unfold((this, that)) {
  //      case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
  //      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
  //      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
  //      case _ => None
  //    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) ->(t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  /**
   * 5.14
   *
   * Hard: Implement startsWith using functions you’ve written. It should check if one
   * Stream is a prefix of another. For instance, Stream(1,2,3) startsWith Stream(1,2)
   * would be true .
   */
  //  def startsWith[B >: A](s: Stream[B]): Boolean =
  //    (this, s) match {
  //      case (_, Empty) => true
  //      case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWith(t2())
  //      case _ => false
  //    }

  /*
  `s startsWith s2` when corresponding elements of `s` and `s2` are all equal, until the point that
  `s2` is exhausted. If `s` is exhausted first, or we find an element that doesn't match, we
  terminate early. Using non-strictness, we can compose these three separate logical steps--the
  zipping, the termination when the second stream is exhausted, and the termination if a
  non matching element is found or the first stream is exhausted.
  */
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined) forAll {
      case (h, h2) => h == h2
    }

  /**
   * 5.15
   *
   * Implement tails using `unfold`. For a given `Stream`, `tails` returns the `Stream` of suffixes
   * of the input sequence, starting with the original `Stream`. For example, given `Stream(1,2,3)`
   * it would return `Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())`.
   */
  def tails(): Stream[Stream[A]] =
    unfold(this) {
      case s @ Cons(h, t) => Some(s, t())
      case Empty => None
    } append Stream(Empty)

  def hasSubsequence[B >: A](s: Stream[B]): Boolean =
    tails exists (_ startsWith s)

  /**
   * 5.16
   *
   * Hard: Generalize `tails` to the function `scanRight`, which is like a `foldRight` that
   * returns a stream of the intermediate results.
   *
   * Can it be implemented using unfold ? How, or why not ? Could it be implemented using another
   * function we’ve written?
   *
   * My answer: yes, it can be implemented using `unfold` and `foldRight`
   */

  /** my answer has correct result but different evaluation order */
  //  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
  //    (unfold(this) {
  //      case s @ Cons(h, t) => Some(s, t())
  //      case Empty => None
  //    } append Stream(Empty)) map (_.foldRight(z)(f))

  /*
  The function can't be implemented using `unfold`, since `unfold` generates elements of the
  `Stream` from left to right. It can be implemented using `foldRight` though.

  The implementation is just a `foldRight` that keeps the accumulated value and the stream of
  intermediate results, which we `cons` onto during each iteration. When writing folds, it's common
  to have more state in the fold than is needed to compute the result. Here, we simply extract the
  accumulated list once finished.
  */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure
      // only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
}

case object Empty extends Stream[Nothing]

/**
 * The constructor parameters are thunks, instead of call-by-name, because `val`
 * parameters may not be call-by-name. The reason might be the following:
 *
 * A val with call-by-name is a def. Since Scala does not guarantee immutability,
 * evaluating an expression multiple times may lead to different values,
 * which is not acceptable for stable identifiers like `val`s.
 *
 * The closest you can get is a `lazy val`. You cannot use that in a
 * case class constructor but I can't think of a good reason why it's
 * disallowed (other than that it wouldn't be very useful -- all of the
 * interesting case class features would need to force evaluation).
 */
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    /*
     * Even if `head` and `tail` are used in thunks, they will not be evaluated,
     * because thunks remains unevaluated until we force the evaluation
     */
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  /**
   * 5.11
   *
   * Write a more general stream-building function called unfold. It takes an initial state,
   * and a function for producing both the next state and the next value in the generated
   * stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((value, state)) => Stream.cons(value, unfold(state)(f))
      case None => Empty
    }
}