package me.invkrh.fpis.ch6

/**
 * Created with IntelliJ IDEA.
 * User: invkrh
 * Date: 9/9/15
 * Time: 9:53 PM
 */

/**
 * 6.11
 *
 * Hard: To gain experience with the use of State, implement a finite state automaton
 * that models a simple candy dispenser. The machine has two types of input: you can
 * insert a coin, or you can turn the knob to dispense candy. It can be in one of two
 * states: locked or unlocked. It also tracks how many candies are left and how many
 * coins it contains.
 */
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

/**
 * Rules:
 *
 * Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
 *
 * Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
 *
 * Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
 *
 * A machine that’s out of candy ignores all inputs.
 */


object Machine {
  /**
   * The method simulateMachine should operate the machine based on the list of inputs
   * and return the number of coins and candies left in the machine at the end. For example,
   * if the input Machine has 10 coins and 5 candies, and a total of 4 candies are successfully
   * bought, the output should be (14, 1).
   */

  //  def trans(input: Input) = {
  //    State[Machine, (Int, Int)] {
  //      m =>
  //        if (m.candies <= 0) ((m.coins, m.candies), m)
  //        else input match {
  //          case Coin if m.locked =>
  //            ((m.coins + 1, m.candies), Machine(locked = false, m.candies, m.coins + 1))
  //          case Turn if !m.locked =>
  //            ((m.coins, m.candies - 1), Machine(locked = true, m.candies - 1, m.coins))
  //          case _ => ((m.coins, m.candies), m)
  //        }
  //    }
  //  }
  //
  //  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
  //    (inputs map trans).reduceLeft {
  //      (a, b) => a.map2(b) {
  //        (va, vb) => vb
  //      }
  //    }
  //  }

  import State._

  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    for {
      _ <- sequence(inputs map (modify[Machine] _ compose update))
      s <- get
    } yield (s.coins, s.candies)

    sequence(inputs map (modify[Machine] _ compose update)).flatMap {
      _ => get.map(s => (s.coins, s.candies))
    }
  }
}