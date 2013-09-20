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
    def doBalance(chars: List[Char], isBalanced: Boolean): Boolean = {
      if (chars.isEmpty)
         isBalanced
      else {
        val left = chars.head == '('
        val right = chars.head == ')'
        val balanced = !left || (right && !isBalanced)
        doBalance(chars.tail, balanced)
      }
    }

    doBalance(chars, true)
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
