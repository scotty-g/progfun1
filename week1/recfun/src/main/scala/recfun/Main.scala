package recfun

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
    if (c == 0 && r == 0) 1
    else if ((c - r) == 0) 0 + pascal(c - 1, r - 1)
    else if (c - 1 < 0) 0 + pascal(c, r - 1)
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def calcIndex(c: Char): Int = c match {
      case '(' => 1
      case ')' => -1
      case _ => 0
    }

    def balance2(acc: Int, chars: List[Char]): Int = {
      if (chars.isEmpty) acc
      else {
        val i = calcIndex(chars.head)
        if (acc + i < 0) acc - 1
        else balance2(acc + i, chars.tail)
      }
    }

    balance2(0, chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money < 0) 0
    else if(coins.isEmpty)
      if(money == 0) 1 else 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
