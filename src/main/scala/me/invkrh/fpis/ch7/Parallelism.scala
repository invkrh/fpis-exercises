package me.invkrh.fpis.ch7

import java.util.concurrent.{Callable, Future, ExecutorService, TimeUnit}

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 9/13/15
 * Time: 11:02 PM
 */


object Parallelism {

  type Par[A] = ExecutorService => Future[A]

  /**
   * 7.2
   *
   * Before continuing, try to come up with representations for `Par` that make it possible
   * to implement the functions of our API.
   */
  object Par {

    // promotes a constant value to a parallel computation.
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    /**
     * 7.1
     *
     * `Par.map2` is a new higher-order function for combining the result of two parallel
     * computations.
     */
    //  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    //    (es: ExecutorService) => {
    //      val af = a(es)
    //      val bf = b(es)
    //      UnitFuture(f(af.get, bf.get))
    //    }

    /**
     * 7.3
     * Hard: Fix the implementation of map2 so that it respects the contract of timeouts on
     * `Future`.
     */
    // combines the results of two parallel computations with a binary function.
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      es => {
        val (af, bf) = (a(es), b(es))
        Map2Future(af, bf, f)
      }

    /*
     * Note: this implementation will not prevent repeated evaluation if multiple threads call `get`
     * in parallel. We could prevent this using synchronization, but it isn't needed for our
     * purposes here (also, repeated evaluation of pure values won't affect results).
     */
    case class Map2Future[A, B, C](a: Future[A], b: Future[B],
                                   f: (A, B) => C) extends Future[C] {
      @volatile var cache: Option[C] = None

      def isDone = cache.isDefined

      def isCancelled = a.isCancelled || b.isCancelled

      def cancel(evenIfRunning: Boolean) =
        a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

      def get = compute(Long.MaxValue)

      def get(timeout: Long, units: TimeUnit): C =
        compute(TimeUnit.NANOSECONDS.convert(timeout, units))

      private def compute(timeoutInNanos: Long): C = cache match {
        case Some(c) => c
        case None =>
          val start = System.nanoTime
          val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
          val stop = System.nanoTime
          val aTime = stop - start
          val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
          val ret = f(ar, br)
          cache = Some(ret)
          ret
      }
    }

    // marks a computation for concurrent evaluation.
    // The evaluation won’t actually occur until forced by run.
    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })

    // wraps its unevaluated argument in a Par and marks it for concurrent evaluation.
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    // extracts a value from a `Par` by actually performing the computation.
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    /**
     * 7.4
     *
     * Using `lazyUnit`, write a function to convert any function `A => B` to one that evaluates its
     * result asynchronously.
     */
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

    /**
     * 7.5
     *
     * Hard: Write this function, called `sequence`. No additional primitives are required. Do
     * not call run.
     */
    def sequenceSimple[A](ps: List[Par[A]]): Par[List[A]] =
      ps.foldRight(unit(List[A]())) {
        case (h, t) => map2(h, t)(_ :: _)
      }

    // This implementation forks the recursive step off to a new logical thread,
    // making it effectively tail-recursive. However, we are constructing
    // a right-nested parallel program, and we can get better performance by
    // dividing the list in half, and running both halves in parallel.
    // See `sequenceBalanced` below.
    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
      }

    // We define `sequenceBalanced` using `IndexedSeq`, which provides an
    // efficient function for splitting the sequence in half.
    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    /**
     * 7.6
     *
     * Implement `parFilter`, which filters elements of a list in parallel.
     */
    //    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    //      val fas: List[Par[A]] = as.map(asyncF(x => x))
    //      fas.foldRight(unit(List[A]())) {
    //        case (h, t) =>
    //          map2(h, t) {
    //            case (hv, tv) => if (f(hv)) hv :: tv else tv
    //          }
    //      }
    //    }
    def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars: List[Par[List[A]]] =
        l map asyncF((a: A) => if (f(a)) List(a) else List())
      map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of
      // lists
    }

    /**
     * Optional:
     *
     * Write a more general version of the parallel summation function we wrote at the beginning of
     * this chapter. Try using it to find the maximum value of an `IndexedSeq` in parallel
     */
    def reduce(ints: IndexedSeq[Int])(f: (Int, Int) => Int): Par[Int] = {
      if (ints.length <= 1)
        Par.unit(ints.headOption getOrElse 0)
      else {
        val (l, r) = ints.splitAt(ints.length / 2)
        Par.map2(Par.fork(reduce(l)(f)), Par.fork(reduce(r)(f)))(f)
      }
    }

    def sum(ints: IndexedSeq[Int]): Par[Int] = reduce(ints)(_ + _)

    def max(ints: IndexedSeq[Int]): Par[Int] = reduce(ints)((a, b) => scala.math.max(a, b))

    /**
     * Optional:
     *
     * Write a function that takes a list of paragraphs (a `List[String]`) and returns the total
     * number of words across all paragraphs, in parallel. Generalize this function as much as
     * possible.
     */
    def count[A](as: List[A])(f: A => Int): Par[Int] = {
      val fas = as map asyncF(a => f(a))
      map(sequence(fas))(_.sum)
    }

    /**
     * Optional:
     *
     * Implement `map3`, `map4`, and `map5`, in terms of `map2`.
     */
    def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
      val fc = map2(a, b)((va: A, vb: B) => (vc: C) => f(va, vb, vc))
      map2(c, fc)((vc, func) => func(vc))
    }

    def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E)
    : Par[E] = {
      val fcd = map2(a, b)((va: A, vb: B) => (vc: C, vd: D) => f(va, vb, vc, vd))
      val fd = map2(c, fcd)((vc, func) => (d: D) => func(vc, d))
      map2(d, fd)((vd, func) => func(vd))
    }

    def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])
                              (f: (A, B, C, D, E) => F): Par[F] = {
      val fcde = map2(a, b)((va: A, vb: B) => (vc: C, vd: D, ve: E) => f(va, vb, vc, vd, ve))
      val fde = map2(c, fcde)((vc, func) => (d: D, e: E) => func(vc, d, e))
      val fe = map2(d, fde)((vd, func) => (e: E) => func(vd, e))
      map2(e, fe)((ve, func) => func(ve))
    }

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get

    def delay[A](fa: => Par[A]): Par[A] =
      es => fa(es)

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es =>
        if (run(es)(cond).get) t(es)
        else f(es)

    /**
     * 7.11
     *
     * Implement choiceN and then choice in terms of choiceN .
     */
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
      es =>
        val idx = run(es)(n).get
        choices(idx)(es)
    }

    def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
      es =>
        val index = map(cond) {
          bool => if (bool) 0 else 1
        }
        choiceN(index)(List(t, f))(es)
    }

    /**
     * 7.12
     *
     * There’s still something rather arbitrary about `choiceN`. The choice of List seems
     * overly specific. Why does it matter what sort of container we have? For instance,
     * what if, instead of a list of computations, we have a Map of them
     */
    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = {
      es =>
        val k = run(es)(key).get
        choices(k)(es)
    }

    /**
     * 7.13
     *
     * Implement this new primitive chooser , and then use it to implement choice and choiceN.
     */
    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
      es =>
        val ref = run(es)(pa).get
        choices(ref)(es)
    }

    def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
      es =>
        chooser(cond) {
          bool =>
            if (bool) t else f
        }(es)
    }

    def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
      es =>
        chooser(n) {
          index => choices(index)
        }(es)
    }

    /**
     * 7.14
     *
     * Implement `join`. Can you see how to implement flatMap using `join`? And can you
     * implement join using `flatMap`?
     */
    def join[A](a: Par[Par[A]]): Par[A] = {
      es =>
        run(es)(run(es)(a).get())
    }

    def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {
      join(map(a)(f))
    }

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = {
      flatMap(a)(x => x)
    }

    /**
     * 7.15
     *
     * implement a function with the same signature as `map2`, but using `flatMap` and `unit`?
     * How is its meaning different than that of `map2`?
     */
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
      flatMap(a) {
        va => flatMap(b) {
          vb => unit(f(va, vb))
        }
      }
    }
  }
}
