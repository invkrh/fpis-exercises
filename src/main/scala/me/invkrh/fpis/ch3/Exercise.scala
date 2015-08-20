package me.invkrh.fpis.ch3

import List._

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 8/5/15
 * Time: 12:33 AM
 */

object Exercise {

  /**
   * 3.1
   * the result of the following match expression
   */
  val res_3_1 = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  /**
   * 3.2
   * Implement the function tail for removing the first element of a List . 
   * Note that the function takes constant time.
   */
  def tail[T](l: List[T]): List[T] = l match {
    //    case Nil => Nil
    //    case Cons(x, xs) => xs
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  /**
   * 3.3
   * Using the same idea, implement the function setHead for replacing the first element
   * of a List with a different value
   */
  def setHead[T](l: List[T], h: T): List[T] = l match {
    //    case Nil => Nil
    //    case Cons(x, xs) => Cons(h, xs)
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  /**
   * 3.4
   * Generalize tail to the function drop, which removes the first n elements from a list.
   * Note that this function takes time proportional only to the number of elements being
   * dropped—we don’t need to make a copy of the entire List .
   */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else {
      l match {
        case Nil => Nil
        //case Cons(x, xs) => drop(xs, n - 1) // need to ignore head
        case Cons(_, xs) => drop(xs, n - 1)
      }
    }
  }

  /**
   * 3.5
   * Implement dropWhile, which removes elements from the List prefix as long as they
   * match a predicate.
   */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => l
  }

  /**
   * 3.6
   * Implement a function, init, that returns a List consisting of all
   * but the last element of a List.
   */

  /**
   * Key:
   *
   * Note that we're copying the entire list up until the last element. Besides being
   * inefficient, the natural recursive solution will use a stack frame for each element of the
   * list, which can lead to stack overflows for large lists (can you see why?). With lists, it's
   * common to use a temporary, mutable buffer internal to the function (with lazy lists or streams,
   * which we discuss in chapter 5, we don't normally do this). So long as the buffer is allocated
   * internal to the function, the mutation is not observable and RT is preserved.
   *
   * Another common convention is to accumulate the output list in reverse order,
   * then reverse it at the end, which doesn't require even local mutation. We'll write a reverse
   * function later in this chapter.
   */
  def init[A](l: List[A]): List[A] = l match {
    //case Nil => Nil
    case Nil => sys.error("init of empty list")
    //case Cons(x, Nil) => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }
    go(l)
  }

  /**
   * 3.7 - uncertain
   * Can product, implemented using foldRight, immediately halt the recursion and
   * return 0.0 if it encounters a 0.0. Consider how any short-circuiting
   * might work if you call foldRight with a large list.
   *
   * `f` is first evaluated for the last 2 elements, even through those before them
   * containing 0, the recursive has to be finished.
   */

  /**
   * Key:
   *
   * No, this is not possible! The reason is because _before_ we ever call our function, `f`,
   * we evaluate its argument, which in the case of `foldRight` means traversing the list
   * all the way to the end. We need _non-strict_ evaluation to support early termination
   * ---we discuss this in chapter 5.
   */
  def product(ds: List[Double]): Double = ???

  /**
   * 3.8
   * See what happens when you pass Nil and Cons themselves to foldRight.
   * What do you think this says about the relationship between foldRight and the data
   * constructors of List ?
   *
   * They are equivalent.
   */

  /**
   * Key:
   *
   * We get back the original list! Why is that? As we mentioned earlier,
   * one way of thinking about what `foldRight` "does" is it replaces the `Nil` constructor
   * of the list with the `z` argument, and it replaces the `Cons` constructor with the given
   * function, `f`. If we just supply `Nil` for `z` and `Cons` for `f`,
   * then we get back the input list.
   */
  val res_3_8 = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

  /**
   * 3.9
   * Compute the length of a list using foldRight .
   */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0) {
      // case (elem, cnt) => cnt + 1
      case (_, cnt) => cnt + 1
    }
  }

  /**
   * 3.10
   * Write another general list-recursion function, `foldLeft`, that is tail-recursive
   */
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  /**
   * 3.11
   * Write sum, product, and a function to compute the length of a list using foldLeft.
   */
  object WithFoldLeft {
    def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

    //def product(l: List[Int]): Int = foldLeft(l, 1)(_ * _)
    def product(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

    def length[A](as: List[A]): Int = {
      foldLeft(as, 0) {
        //case (cnt, elem) => cnt + 1
        case (cnt, _) => cnt + 1
      }
    }
  }

  /**
   * 3.12
   * Write a function that returns the reverse of a list.
   * Given List(1,2,3) it returns List(3,2,1) ).
   */
  def reverse[T](l: List[T]): List[T] = foldLeft(l, List[T]()) {
    case (lst, elem) => Cons(elem, lst)
  }

  /**
   * 3.13
   * Write foldLeft in terms of foldRight, and the other way around?
   */

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case l@Cons(x, xs) =>
      foldLeft(reverse(l), z) {
        case (b, a) => f(a, b)
      }
  }

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)


  /**
   * Key:
   */
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  // foldLeft processes items in the reverse order from foldRight.  It's
  // cheating to use reverse() here because that's implemented in terms of
  // foldLeft!  Instead, wrap each operation in a simple identity function to
  // delay evaluation until later and stack (nest) the functions so that the
  // order of application can be reversed.  We'll call the type of this
  // particular identity/delay function BtoB so we aren't writing B => B
  // everywhere:

  //  object FoldLeft {
  //    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
  //      case Nil => z
  //      case l@Cons(x, xs) =>
  //        List.foldRight(reverse(l), z) {
  //          case (a, b) => f(b, a)
  //        }
  //    }
  //  }

  // Here is the same function with much more description
  def foldLeftViaFoldRight_1[A, B](as: List[A], outerIdent: B)(combiner: (B, A) => B): B = {

    type BtoB = B => B

    // Here we declare a simple instance of BtoB according to the above
    // description.  This function will be the identity value for the inner
    // foldRight.
    def innerIdent: BtoB = (b: B) => b

    // For each item in the 'as' list (the 'a' parameter below), make a new
    // delay function which will use the combiner function (passed in above)
    // when it is evaluated later.  Each new function becomes the input to the
    // previous function (delayFunc).
    //
    //                        This much is just the type signature
    //                  ,-------^-------.
    def combinerDelayer: (A, BtoB) => BtoB =
      (a: A, delayFunc: BtoB) => (b: B) => delayFunc(combiner(b, a))
    // `----------v---------'    `----------------v---------------'
    //         Paramaters                 The returned function

    // Pass the original list 'as', plus the simple identity function and the
    // new combinerDelayer to foldRight.  This will create the functions for
    // delayed evaluation with an combiner inside each one, but will not
    // apply any of those functions.
    def go: BtoB = foldRight(as, innerIdent)(combinerDelayer)

    // This forces all the evaluations to take place
    go(outerIdent)
  }


  /**
   * 3.14
   * Implement append in terms of either foldLeft or foldRight .
   */
  //  def append[T](l: List[T], newElem: T) =
  //    foldRight(l, List(newElem)) {
  //      case (elem, lst) => Cons(elem, lst)
  //    }

  /**
   * Keys:
   *
   * `append` simply replaces the `Nil` constructor of the first list with the second list, which is
   * exactly the operation performed by `foldRight`.
   */
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  /**
   * 3.15
   * Write a function that concatenates a list of lists into a single list.
   * Its runtime should be linear in the total length of all lists.
   */
  //  def concat[T](lists: List[List[T]]): List[T] =
  //    foldLeft(lists, List[T]()) {
  //      case (acc, l) => foldLeft(l, acc) {
  //        case (innerAcc, elem) => append(innerAcc, elem)
  //      }
  //    }

  /**
   * Key:
   *
   * Since `append` takes time proportional to its first argument, and this first argument never
   * grows because of the right-associativity of `foldRight`, this function is linear in the total
   * length of all lists. You may want to try tracing the execution of the implementation on paper
   * to convince yourself that this works.
   *
   * Note that we're simply referencing the `append` function, without writing something like `(x,y)
   * => append(x,y)` or `append(_,_)`. In Scala there is a rather arbitrary distinction between
   * functions defined as _methods_, which are introduced with the `def` keyword, and function
   * values, which are the first-class objects we can pass to other functions, put in collections,
   * and so on. This is a case where Scala lets us pretend the distinction doesn't exist. In other
   * cases, you'll be forced to write `append _` (to convert a `def` to a function value) or even
   * `(x: List[A], y: List[A]) => append(x,y)` if the function is polymorphic and the type arguments
   * aren't known.
   */
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(appendViaFoldRight)

  /**
   * 3.16
   * Write a function that transforms a list of integers by adding 1 to each element.
   */
  //  def increment(as: List[Int]) = {
  //    foldRight(as, List[Int]()) {
  //      case (elem, acc) => Cons(elem + 1, acc)
  //    }
  //  }

  def increment(l: List[Int]): List[Int] =
  //use ascription to type parameter, usually it's a upper casting
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  /**
   * 3.17
   * Write a function that turns each value in a List[Double] into a String.
   */
  def double2String(as: List[Double]) = {
    foldRight(as, List[String]()) {
      case (elem, acc) => Cons(elem.toString, acc)
    }
  }

  /**
   * 3.18
   * Write a function map that generalizes modifying each element in a list
   * while maintaining the structure of the list
   */
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, List[B]()) {
      case (elem, acc) => Cons(f(elem), acc)
    }
  }

  /**
   * Key:
   */
  def map_1[A, B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft_1(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def map_2[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  /**
   * 3.19
   * Write a function filter that removes elements from a list unless they satisfy a given
   * predicate. Use it to remove all odd numbers from a List[Int]
   */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List[A]()) {
      case (elem, acc) => if (f(elem)) Cons(elem, acc) else acc
    }
  }

  /**
   * Key:
   */
  def filter_1[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft_1(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h, t) => if (f(h)) buf += h; go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  /**
   * 3.20
   * Write a function flatMap that works like map except that the function given will return
   * a list instead of a single result, and that list should be inserted into the final resulting
   * list.
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  /**
   * 3.21
   * Use flatMap to implement filter .
   */
  object WithFlatMap {
    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      flatMap(as) {
        elem => if (f(elem)) List(elem) else Nil
      }
    }
  }

  /**
   * 3.22
   * Write a function that accepts two lists and constructs a new list
   * by adding corresponding elements.
   */
  def zipAdd(as: List[Int], bs: List[Int]): List[Int] =
  //    as match {
  //      case Nil => bs
  //      case Cons(ha, ta) =>
  //        bs match {
  //          case Nil => as
  //          case Cons(hb, tb) => Cons(ha + hb, zipAdd(ta, tb))
  //        }
  //    }

  //using pair to simplify matching
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAdd(t1, t2))
    }

  /**
   * 3.23
   * Generalize the function you just wrote so that it’s not specific to integers or addition.
   */
  def zipWith[T](as: List[T], bs: List[T])(f: (T, T) => T): List[T] =
  //    as match {
  //      case Nil => bs
  //      case Cons(ha, ta) =>
  //        bs match {
  //          case Nil => as
  //          case Cons(hb, tb) => Cons(f(ha, hb), zipWith(ta, tb)(f))
  //        }
  //    }
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  /**
   * 3.24
   * Implement hasSubsequence for checking whether a List contains another List as a subsequence.
   */
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def checkInit(as: List[A], bs: List[A]): Boolean = {
      (as, bs) match {
        //case (Nil, _) => false
        case (_, Nil) => true
        case (Cons(x, xs), Cons(y, ys)) if x == y => checkInit(xs, ys)
        case _ => false
      }
    }

    //    (sup, sub) match {
    //      case (Nil, _) => false
    //      case (_, Nil) => true
    //      case (Cons(x, xs), _) =>
    //        if (checkInit(sup, sub)) true else hasSubsequence(xs, sub)
    //    }

    sup match {
      case Nil => sub == Nil
      case _ if checkInit(sup, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }

  }

  /**
   * 3.25
   * Write a function size that counts the number of nodes (leaves and branches) in a tree.
   */
  def size[T](t: Tree[T]): Int = t match {
    case Branch(l, r) => size(l) + size(r) + 1 // count self in
    case Leaf(_) => 1
  }

  /**
   * 3.26
   * Write a function maximum that returns the maximum element in a Tree[Int].
   */
  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v) => v
  }

  /**
   * 3.27
   * Write a function depth that returns the maximum path length from the root of
   * a tree to any leaf.
   */
  def depth[T](t: Tree[T]): Int = t match {
    case Branch(l, r) => 1 + (depth(l) max depth(r))
    case Leaf(v) => 0
  }

  /**
   * 3.28
   * Write a function map, analogous to the method of the same name on List,
   * that modifies each element in a tree with a given function.
   */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v) => Leaf(f(v))
  }

  /** 3.9
    * Generalize size, maximum, depth, and map, writing a new function fold that abstracts
    * over their similarities. Reimplement them in terms of this more general function.
    */
  def fold[A, B](t: Tree[A])(f: (A) => B)(g: (B, B) => B): B = t match {
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(v) => f(v)
  }

  object WithTreeFold {
    def size[T](t: Tree[T]): Int = fold(t)(a => 1)(1 + _ + _)

    //      fold(t) {
    //        case elem => 1
    //      } {
    //        case (l, r) => l + r
    //      }

    def maximum(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

    //      fold(t) {
    //        case elem => elem
    //      } {
    //        case (l, r) => l max r
    //      }

    def depth[T](t: Tree[T]): Int = fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

    //      fold(t) {
    //        case elem => 0
    //      } {
    //        case (l, r) => (l + 1) max (r + 1)
    //      }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

    //    fold(t) {
    //      case elem => Tree.apply(f(elem))
    //    } {
    //      case (l, r) => Branch(l, r)
    //    }
  }

}
