package me.invkrh.fpis.ch6

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 9/8/15
 * Time: 11:17 PM
 */

/**
 * 6.10
 *
 * Generalize the functions `unit`, `map`, `map2`, `flatMap`, and `sequence`. Add them as methods
 * on the `State` case class where possible. Otherwise you should put them in a `State` companion
 * object.
 */

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State {
      s =>
        val (a, sa) = run(s)
        f(a).run(sa)
    }

  def map[B](f: A => B): State[S, B] =
    flatMap(x => State.unit(f(x)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State[S, A]((a, _))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    val z = unit[S, List[A]](Nil)
    fs.foldRight(z)((f, acc) => f.map2(acc)(_ :: _))
  }

  // The idiomatic solution is expressed via foldRight
  def sequenceViaFoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  // This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could also use a collection.mutable.ListBuffer internally.)
  def sequenceViaLoop[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
      actions match {
        case Nil => (acc.reverse, s)
        case h :: t => h.run(s) match {
          case (a, s2) => go(s2, t, a :: acc)
        }
      }
    State((s: S) => go(s, sas, List()))
  }

  // We can also write the loop using a left fold. This is tail recursive like the
  // previous solution, but it reverses the list _before_ folding it instead of after.
  // You might think that this is slower than the `foldRight` solution since it
  // walks over the list twice, but it's actually faster! The `foldRight` solution
  // technically has to also walk the list twice, since it has to unravel the call
  // stack, not being tail recursive. And the call stack will be as tall as the list
  // is long.
  def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

}
