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
      return 1;
    } else {
      return pascal(c - 1, r - 1) + pascal(c, r - 1);
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def doBalance(chars: List[Char], depth: Int, isBalanced: Boolean): Boolean = {
      if (chars.isEmpty)
        return (depth == 0) && isBalanced

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

    return doBalance(chars, 0, true)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0 || coins.isEmpty)
      return 0

    var waysToMakeChange = 0
    if (coins.head == 0)
      return countChange(money, coins.tail)

    if (money % coins.head == 0)
      waysToMakeChange = waysToMakeChange + 1

    for(goesInto <- 0 to (money / coins.head)) {
      waysToMakeChange = waysToMakeChange + countChange(money - (coins.head * goesInto), coins.tail)
    }

    return waysToMakeChange
  }
}
