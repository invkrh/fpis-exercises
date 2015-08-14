package me.invkrh.ch3

import me.invkrh.ch3.List._

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
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  /**
   * 3.3
   * Using the same idea, implement the function setHead for replacing the first element
   * of a List with a different value
   */
  def setHead[T](l: List[T], v: T): List[T] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(v, xs)
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
        case Cons(x, xs) => drop(xs, n - 1)
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
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
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
  def product(ds: List[Double]): Double = ???

  /**
   * 3.8
   * See what happens when you pass Nil and Cons themselves to foldRight.
   * What do you think this says about the relationship between foldRight and the data
   * constructors of List ?
   *
   * They are equivalent.
   */
  val res_3_8 = foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))

  /**
   * 3.9
   * Compute the length of a list using foldRight .
   */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0) {
      case (elem, cnt) => cnt + 1
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

    def product(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

    def length[A](as: List[A]): Int = {
      foldLeft(as, 0) {
        case (cnt, elem) => cnt + 1
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

  object FoldLeft {
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case l@Cons(x, xs) =>
        List.foldRight(reverse(l), z) {
          case (a, b) => f(b, a)
        }
    }
  }

  /**
   * 3.14
   * Implement append in terms of either foldLeft or foldRight .
   */
  def append[T](l: List[T], newElem: T) =
    foldRight(l, List(newElem)) {
      case (elem, lst) => Cons(elem, lst)
    }

  /**
   * 3.15
   * Write a function that concatenates a list of lists into a single list.
   * Its runtime should be linear in the total length of all lists.
   */
  def concat[T](lists: List[List[T]]): List[T] =
    foldLeft(lists, List[T]()) {
      case (acc, l) => foldLeft(l, acc) {
        case (innerAcc, elem) => append(innerAcc, elem)
      }
    }

  /**
   * 3.16
   * Write a function that transforms a list of integers by adding 1 to each element.
   */
  def increment(as: List[Int]) = {
    foldRight(as, List[Int]()) {
      case (elem, acc) => Cons(elem + 1, acc)
    }
  }

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
  def zipAdd(as: List[Int], bs: List[Int]): List[Int] = {
    as match {
      case Nil => bs
      case Cons(ha, ta) =>
        bs match {
          case Nil => as
          case Cons(hb, tb) => Cons(ha + hb, zipAdd(ta, tb))
        }
    }
  }

  /**
   * 3.23
   * Generalize the function you just wrote so that it’s not specific to integers or addition.
   */
  def zipWith[T](as: List[T], bs: List[T])(f: (T, T) => T): List[T] = {
    as match {
      case Nil => bs
      case Cons(ha, ta) =>
        bs match {
          case Nil => as
          case Cons(hb, tb) => Cons(f(ha, hb), zipWith(ta, tb)(f))
        }
    }
  }

  /**
   * 3.24
   * Implement hasSubsequence for checking whether a List contains another List as a subsequence.
   */
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def checkInit(as: List[A], bs: List[A]): Boolean = {
      bs match {
        case Nil => true
        case Cons(y, ys) =>
          as match {
            case Nil => false
            case Cons(x, xs) => if (x == y) checkInit(xs, ys) else false
          }
      }
    }

    sub match {
      case Nil => true
      case Cons(y, ys) =>
        sup match {
          case Nil => false
          case Cons(x, xs) =>
            if (checkInit(sup, sub)) true else hasSubsequence(xs, sub)
        }
    }
  }

  /**
   * 3.25
   * Write a function size that counts the number of nodes (leaves and branches) in a tree.
   */
  def size[T](t: Tree[T]): Int = t match {
    case Branch(l, r) => size(l) + size(r)
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
    case Branch(l, r) => (depth(l) + 1).max(depth(r) + 1)
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
    def size[T](t: Tree[T]): Int = fold(t) {
      case elem => 1
    } {
      case (l, r) => l + r
    }

    def maximum(t: Tree[Int]): Int = fold(t) {
      case elem => elem
    } {
      case (l, r) => l max r
    }

    def depth[T](t: Tree[T]): Int = fold(t) {
      case elem => 0
    } {
      case (l, r) => (l + 1) max (r + 1)
    }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t) {
      case elem => Tree.apply(f(elem))
    } {
      case (l, r) => Branch(l, r)
    }
  }

}
