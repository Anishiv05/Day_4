object MaxProfit {

  def maxProfit(prices: Array[Int]): Int = {
    var mini = prices(0)
    var profit = 0

    for (i <- 1 until prices.length) {
      val sell = prices(i) - mini
      profit = math.max(profit, sell)
      mini = math.min(mini, prices(i))
    }
    profit
  }


  import scala.annotation.tailrec
  def maxProfitUsingTailRecursion(prices: Array[Int]): Int = {
    @tailrec
    def helper(prices: List[Int], maxProfit: Int, lastBuy: Int): Int = {
      prices match {
        case Nil => maxProfit
        case head :: tail => if (head > lastBuy) {
          helper(tail, Math.max(maxProfit, head - lastBuy), lastBuy)
        } else {
          helper(tail, maxProfit, head)
        }
      }
    }
    val listPrices = prices.toList
    helper(listPrices.tail, 0, listPrices.head)
  }


  def main(args: Array[String]): Unit = {
    val prices = Array(7, 1, 5, 3, 6, 4)
    val result = maxProfit(prices)
    println(s"Maximum Profit: $result \n")

    val result1 = maxProfitUsingTailRecursion(prices)
    println(s"Maximum Profit Using Tail Recursion: $result1")
  }
}