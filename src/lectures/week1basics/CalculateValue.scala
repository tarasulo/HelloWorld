package lectures.week1basics

import scala.annotation.tailrec

object CalculateValue extends App {
  def addY(x: Int, y: Int, n: Int): Unit = {
    var result = x

    @tailrec
    def loop(x: Int, y: Int, n: Int): Int = {
      if (n < 1) result
      else {
        result += y
        loop(x, y, n - 1)
      }
    }

    loop(x, y, n)

    def printResult(result: Int): Unit = {
      val length = result.toString.length
      @tailrec
      def printValue(result: Int, length: Int): Unit = {
        if (length <= 0) print("is the result")
        else {
          print(result + " ")
          printValue(result, length - 1)
        }
      }
      printValue(result, length)
    }
    printResult(result)
  }
  addY(100, 25, 4)


  println(" ")
  def rev(temp: String): String = temp.split(" ").filter(_.nonEmpty).reverse.mkString(" ")
  println(rev("  Hello    my    friend  "))
}

