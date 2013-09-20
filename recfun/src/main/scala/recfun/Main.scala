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
    def doBalance(chars: List[Char], isBalanced: Boolean, depth: Int): Boolean = {
      if (chars.isEmpty)
         isBalanced && (depth == 0)
      else {
        if(chars.head == '(')
          doBalance(chars.tail, false, depth + 1)
        else
          if (chars.head == ')')
            doBalance(chars.tail, !isBalanced || true, depth - 1)
        else
          doBalance(chars.tail, isBalanced, depth)
      }
    }

    doBalance(chars, true, 0)
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
