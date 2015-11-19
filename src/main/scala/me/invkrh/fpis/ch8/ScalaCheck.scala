package me.invkrh.fpis.ch8

import java.util.concurrent.Executors

import me.invkrh.fpis.ch6.{Exercise => ch6ex, SimpleRNG, RNG, State}
import me.invkrh.fpis.ch5.Stream
import me.invkrh.fpis.ch7.Parallelism._

/**
  * Created with IntelliJ IDEA.
  * User: invkrh
  * Date: 10/20/15
  * Time: 11:28 PM
  */

object ScalaCheck {

  case class SGen[+A](forSize: Int => Gen[A]) {
    /**
      * 8.11
      *
      * Not surprisingly, `SGen` at a minimum supports many of the same operations as `Gen`,
      * and the implementations are rather mechanical. Define some convenience functions
      * on `SGen` that simply delegate to the corresponding functions on `Gen`.
      */
    //    def map[B](f: A => B): SGen[B] = SGen(i => forSize(i).map(f))
    //
    //    def flatMap[B](f: A => SGen[B]): SGen[B] = SGen {
    //      i => forSize(i).flatMap { a => f(a).forSize(i) }
    //    }
    //
    //    def listOfN(size: SGen[Int]): SGen[List[A]] = SGen {
    //      i => forSize(i).listOfN(size.forSize(i))
    //    }

    def apply(n: Int): Gen[A] = forSize(n)

    def map[B](f: A => B): SGen[B] =
      SGen(forSize andThen (_ map f))

    def flatMap[B](f: A => Gen[B]): SGen[B] =
      SGen(forSize andThen (_ flatMap f))

    def **[B](s2: SGen[B]): SGen[(A, B)] =
      SGen(n => apply(n) ** s2(n))
  }

  /**
    * 8.3
    *
    * Assuming the following representation of Prop , implement && as a method of Prop .
    */
  //  trait Prop {
  //    def check: Either[(FailedCase, SuccessCount), SuccessCount]
  //    def &&(p: Prop): Prop = new Prop {
  //      override def check: Boolean = this.check && p.check
  //    }
  //  }

  case class Gen[+A](sample: State[RNG, A]) {

    /**
      * Option
      */
    def map[B](f: A => B): Gen[B] = Gen(this.sample.map(f))

    /**
      * Option: Create a new primitive to generate an (Int,Int) pair in some range
      */
    def map2[B, C](gb: Gen[B])(f: (A, B) => C): Gen[C] = {
      Gen {
        this.sample.map2(gb.sample)(f)
      }
    }

    /**
      * 8.6
      *
      * Implement `flatMap`, and then use it to implement this more dynamic version of `listOfN`.
      * Put `flatMap` and `listOfN` in the `Gen` class.
      */

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      Gen(sample.flatMap {
        a => f(a).sample
      })

    def listOfN(n: Int): Gen[List[A]] = Gen(State.sequence(List.fill(n)(this.sample)))

    def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap {
      i => listOfN(i)
    }

    def **[B](g: Gen[B]): Gen[(A, B)] =
      (this map2 g) ((_, _))

    /**
      * 8.10
      *
      * Implement helper functions for converting `Gen` to `SGen`.
      */
    def unsized: SGen[A] = SGen { _ => this }
  }

  /**
    * 8.12
    *
    * Implement a `listOf` combinator that doesn’t accept an explicit size. It should return an
    * `SGen` instead of a `Gen`. The implementation should generate lists of the requested size.
    */
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    i => g.listOfN(i)
  }

  /**
    * 8.4
    *
    * Implement Gen.choose using this representation of Gen. It should generate integers in
    * the range `start` to `stopExclusive`. Feel free to use functions you’ve already written.
    */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(ch6ex.nonNegativeInt).map(_ % (stopExclusive - start) + start))
  }

  /**
    * 8.5
    *
    * Let’s see what else we can implement using this representation of `Gen`. Try implementing
    * `unit`, `boolean`, and `listOfN`.
    */
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(ch6ex.nonNegativeInt).map(_ % 2 == 0))


  def pairInRange(start: Int, stopExclusive: Int) = {
    val singleInRange = choose(start, stopExclusive)
    singleInRange.map2(singleInRange) {
      (a, b) => (a, b)
    }
  }

  /**
    * Option: `Gen[Option[A]]` -> `Gen[A]` and `Gen[A]` -> `Gen[Option[A]]`
    */
  def fromOption[A](gopt: Gen[Option[A]]) = gopt.map {
    case Some(x) => x
    case None => null
  }

  def toOption[A](g: Gen[A]) = g.map(Option(_))

  /**
    * Option: generate Strings somehow using our existing primitives
    * ASCII code of printable char => Int range map to Char + )
    */
  def rdString(length: Int): String = {
    val gChar = choose(32, 126).map(_.toChar) // printable Char range
    val chars = gChar.listOfN(length).sample.run(SimpleRNG())._1
    chars.mkString
  }

  /**
    * 8.7
    *
    * Implement `union`, for combining two generators of the same type into one, by pulling
    * values from each generator with equal likelihood.
    */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap {
      x => if (x) g1 else g2
    }

  /**
    * 8.8
    *
    * Implement `weighted`, a version of `union` that accepts a weight for each Gen and generates
    * values from each Gen with probability proportional to its weight.
    */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    Gen(State(ch6ex.double)).flatMap {
      x =>
        val w1 = g1._2
        val w2 = g2._2
        if (x < w1 / (w1 + w2)) g1._1 else g2._1
    }


  type FailedCase = String
  type SuccessCount = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  type MaxSize = Int
  type TestCases = Int
  //type Result = Option[(FailedCase, SuccessCount)]
  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

    /**
      * 8.9
      *
      * Now that we have a representation of `Prop`, implement `&&` and `||` for composing `Prop`
      * values. Notice that in the case of failure we don’t know which property was responsible,
      * the left or the right. Can you devise a way of handling this, perhaps by allowing
      * `Prop` values to be assigned a tag or label which gets displayed in the event of a failure?
      */
    def &&(p: Prop): Prop = Prop {
      (max, n, rng) => run(max, n, rng) match {
        case Passed | Proved => p.run(max, n, rng)
        case x => x
      }
    }

    def ||(p: Prop): Prop = Prop {
      (max, n, rng) => run(max, n, rng) match {
        // In case of failure, run the other prop.
        case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
        case x => x
      }
    }

    def tag(msg: String) = Prop {
      (max, n, rng) => run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
    }
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${
        e.getMessage
      }\n" +
      s"stack trace:\n ${
        e.getStackTrace.mkString("\n")
      }"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)


  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  val smallInt = choose(-10, 10)
  val maxProp  = forAll(listOf(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  /**
    * 8.13
    *
    * Define `listOf1` for generating nonempty lists, and then update
    * your specification of max to use this generator.
    */
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen {
    i => g.listOfN(i)
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n max 1))

  val maxProp1 = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  /**
    * 8.14
    *
    * Write a property to verify the behavior of List.sorted
    */

  //  val sortedProp1 = forAll(listOf1(smallInt)) { ns =>
  //    def checkSorted(l: List[Int]): Boolean = {
  //      l match {
  //        case Nil => true
  //        case x :: xs =>
  //          if (x <= xs.min)  {
  //            checkSorted(xs)
  //          } else false
  //      }
  //    }
  //    checkSorted(ns.sorted)
  //  }

  val sortedProp = forAll(listOf1(smallInt)) { ns =>
    val nss = ns.sorted
    // We specify that every sorted list is either empty, has one element,
    // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
    nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
      case (a, b) => a > b
    } &&
      // Also, the sorted list should have all the elements of the input list,
      !ns.exists(!nss.contains(_)) &&
      // and it should have no elements not in the input list.
      !nss.exists(!ns.contains(_))
  }


  /**
    * 8.15
    *
    * See answer
    */


  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25)

  object ** {
    def unapply[A, B](p: (A, B)) = Some(p)
  }

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p, p2)(_ == _)

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(unit(()))(_ => p)

  val p2 = checkPar {
    equal(
      Par.map(Par.unit(1))(_ + 1),
      Par.unit(2)
    )
  }

  /**
    * 8.16
    *
    * Write a richer generator for `Par[Int]`, which builds more deeply nested parallel
    * computations than the simple ones we gave previously.
    */
  val pint : Gen[Par[Int]] = choose(0, 10) map Par.unit
  val pint2: Gen[Par[Int]] = choose(-100, 100)
    .listOfN(choose(0, 20)).map(l =>
    l.foldLeft(Par.unit(0))((p, i) =>
      Par.fork {
        Par.map2(p, Par.unit(i))(_ + _)
      }))

  /**
    * 8.17
    *
    * Express the property about fork from chapter 7, that `fork(x) == x`
    */
  val forkProp = forAllPar(pint2)(n => equal(Par.fork(n), n)) tag "fork"

  /**
    * 8.18
    *
    * Come up with some other properties that `takeWhile` should satisfy. Can you think of a
    * good property expressing the relationship between `takeWhile` and `dropWhile`?
    */
  val s                 = List(1, 2, 3, 4, 5, 6)
  val p: Int => Boolean = _ < 2.5
  s.takeWhile(p).forall(p) == true
  s.takeWhile(p) ::: s.dropWhile(p) == s

  /**
    * 8.19
    */
  trait Cogen[-A] {
    def sample(a: A, rng: RNG): RNG
  }

  def fn[A, B](in: Cogen[A])(out: Gen[B]): Gen[A => B] = Gen {
    State { (rng: RNG) =>
      val (seed, rng2) = rng.nextInt
      val f = (a: A) => out.sample.run(
        SimpleRNG(seed.toLong ^ in.sample(a, rng).nextInt._1))._1
      (f, rng2)
    }
  }

}
