package recfun
import common._

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
    if (c == 0 || r == 0 || c == r)  {
       1
    } else {
       pascal(c - 1, r - 1) + pascal(c, r - 1);
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def doBalance(chars: List[Char], depth: Int, isBalanced: Boolean): Boolean = {
      if (chars.isEmpty)
         (depth == 0) && isBalanced
      else {
        var newDepth = depth
        var currentBalance = isBalanced
        if (chars.head == '(')  {
          newDepth = depth + 1
        }
        if (chars.head == ')') {
          newDepth = depth - 1
          // but if we were balanced (depth == 0) we're now unbalanced
          // and we'll never be balanced again
          if (newDepth < 0)
            currentBalance = false;
        }
        doBalance(chars.tail, newDepth, currentBalance)
      }
    }

     doBalance(chars, 0, true)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (coins.isEmpty || money < 0)
      0
    else
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
