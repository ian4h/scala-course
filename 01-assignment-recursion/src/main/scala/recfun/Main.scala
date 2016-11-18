package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row) {
        //        println(col+","+row)
        print(pascal(col, row) + " ")
      }
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 && r == 0) 1
    else if (c < 0 || r < 0) 0
    else if (c > r) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def balanced(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) open == 0
      else if (chars.head == '(') balanced(chars.tail, open + 1)
      else if (chars.head == ')') open > 0 && balanced(chars.tail, open - 1)
      else balanced(chars.tail, open)
    }
    @tailrec
    def balance(chars: List[Char], balanced: Int): Boolean = {
      if (chars.isEmpty) balanced == 0
      else {
          if (chars.head == '(') balance(chars.tail, balanced + 1)
          else if (chars.head == ')') balanced > 0 && balance(chars.tail, balanced - 1)
          else balance(chars.tail, balanced)
      }
    }
    balance(chars, 0)
  }

  /**
    * Exercise 3
    * Write a recursive function that counts how many different ways you can make change for an amount, given a list
    * of coin denominations. For example, there are 3 ways to give change for 4 if you have coins with denomination
    * 1 and 2: 1+1+1+1, 1+1+2, 2+2.

      Do this exercise by implementing the countChange function inMain.scala. This function takes an amount to change,
      and a list of unique denominations for the coins. Its signature is as follows:
      def countChange(money: Int, coins: List[Int]): Int
      Once again, you can make use of functions isEmpty, head and tail on the list of integers coins.

      Hint: Think of the degenerate cases. How many ways can you give change for 0 CHF(swiss money)? How many ways can
      you give change for >0 CHF, if you have no coins?
    *
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0 || coins.isEmpty) 0
    else 1

    def loop(money: Int, coins: List[Int]): Int = {


    }
  }
}
