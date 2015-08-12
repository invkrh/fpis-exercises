package me.invkrh.ch3

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 8/12/15
 * Time: 1:01 AM
 */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def apply[A](a: A): Tree[A] = Leaf(a)
}
