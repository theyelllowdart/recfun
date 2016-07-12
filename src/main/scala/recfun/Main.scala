package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || c > r || r < 0) {
      // base case: outside of triangle
      0
    } else if (c == 0 || c == r) {
      // base case: edge of triangle
      1
    } else {
      // iterative case
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balance(unsatisfiedLeftParens: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) {
        // base case: no more elements
        unsatisfiedLeftParens == 0
      } else {
        val head = chars.head
        if (head == '(') {
          // iterative case: consume left paren
          balance(unsatisfiedLeftParens + 1, chars.tail)
        } else if (head == ')') {
          // iterative case: consume right paren
          if (unsatisfiedLeftParens > 0) {
            balance(unsatisfiedLeftParens - 1, chars.tail)
          } else {
            false
          }
        } else {
          // iterative case: consume non paren
          balance(unsatisfiedLeftParens, chars.tail)
        }
      }
    }
    balance(0, chars)
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) {
      // base case: no more coins OR negative money
      0
    } else if (money == 0) {
      // base case: exact change made
      1
    } else {
      // iterative case: use current coin OR switch to next coin
      val coin = coins.head
      countChange(money - coin, coins) + countChange(money, coins.tail)
    }
  }
}
