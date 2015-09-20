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
  def pascal(c: Int, r: Int): Int = 
    if (r == 0 || c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
      
      def balance(chars: List[Char], openedP: Int): Int = {
        def openedPInternal = openedP
        if (chars.isEmpty) {
          return openedPInternal
        }
        
        if (chars.head == ')') {
          if (openedPInternal < 1)
            return 1
          else
            return balance(chars.tail, openedPInternal - 1)
        } else if (chars.head == '(') 
          return balance(chars.tail, openedPInternal + 1)
        
        return balance(chars.tail, openedPInternal)
      }
            
      val openedCnt = balance(chars, 0)
      openedCnt == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money == 0) 1
    else {
      val currentCoin = coins.head    
      if (money < currentCoin) countChange(money, coins.tail)
      else if (money >= currentCoin) 
        countChange(money, coins.tail) + countChange(money - currentCoin, coins)
      else 0;
    }
  }
}
