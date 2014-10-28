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
    if(c == 0 || c == r) 1
  else{
    val aboveLeft = pascal(c-1,r-1)
    val aboveRight = pascal(c,r-1)
    aboveLeft + aboveRight
  }
 }

    /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val openCounter = 0;
      val closerCounter = 0;
      def innerFunc(oc:Int, cc:Int, tails: List[Char]):Boolean = {
        if(tails.isEmpty && oc == cc) return true
        else if(tails.isEmpty && oc != cc)return false
        var localOc = oc
        var localcc = cc
        if(tails.head == ('(')) localOc += 1
        else if(tails.head == (')')) localcc += 1
        if(localcc > localOc) return false
        else innerFunc(localOc,localcc,tails.tail)
      }

      if(chars.isEmpty) return true
      else return innerFunc(openCounter, closerCounter, chars)
    }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    val newcoins = coins.sortWith(_<_)

    def recCount(refactorMoney: Int, newcoins: List[Int]): Int = {

      //exit blocks
      if(refactorMoney == 0) 1

      else if(refactorMoney < 0) 0

      else if(newcoins.isEmpty && refactorMoney>=1 ) 0

      //recursion block
      else return (
        recCount(refactorMoney - newcoins.head, newcoins)
          + recCount(refactorMoney, newcoins.tail)
        )
    }

    recCount(money, newcoins)}
}
