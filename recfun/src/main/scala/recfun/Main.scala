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
    if (r == 0 && c == 0) 1
    else if (c < 0 || c > r) 0
    else pascal(c-1, r-1)+pascal(c, r-1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def f(chars: List[Char], level: Int): Int = {
      if (chars.isEmpty)
        level
      else {
        chars.head match {
          case '(' => f(chars.tail, level+1)
          case ')' => if (level == 0) -1 else f(chars.tail, level-1)
          case _ => f(chars.tail, level)
        }
      }
    }
    f(chars, 0) == 0
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def f(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else {
        val input = "\""+s"f(${money}, [${coins.mkString(",")}])"+"\""
        val out1  = "\""+s"f(${money-coins.head}, [${coins.mkString(",")}])"+"\""
        val out2  = "\""+s"f($money, [${coins.tail.mkString(",")}])"+"\""
        println(s"$input -> $out1")
        println(s"$input -> $out2")
        f(money, coins.tail) + f(money - coins.head, coins)
      }
    }

    if (money == 0 || coins.isEmpty) 0
    else f(money, coins)
  }
}
