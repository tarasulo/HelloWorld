
def square(x: Int) = x * x
square(5)

/*
  Write a function to return a pattern using tail recursion.
  The function should first reduce input value by 4 one by one until reach a negative or 0.
  After reaching 0 or negative, function should add 4 until reach n.
  Example: Input: n = 13 Output: 13,9,5,1,-3,1,5,9,13.
*/

import scala.annotation.tailrec
def calculate(n: Int): Unit = {
  val m = n
  @tailrec
  def tailFunc(n:Int, m:Int, flag:Boolean) :Unit = {
    print(m + ", ")
    if (flag) {
      if (m - 4 > 0) tailFunc(n, m - 4, flag =true)
      else tailFunc(n, m - 4, flag = false)
    } else if (n != m) tailFunc(n, m + 4, flag = false)
  }
  tailFunc(n, m, flag =true)
}
calculate(13)
calculate(16)

/*
  Write a retry function.
  First parameter is function to retry.
  Second is function to validate the result.
  Third is a retries list which specifies count of retries and the pauses between attempts.
*/

@tailrec
def retry[T](block: () => T,
             accept: T => Boolean,
             retries: List[Long]): T = {

  val result = block()
  if (accept(result)) {
    println("Retry success " + result)
    result
  }
  else {
    if (retries.nonEmpty) {
      Thread.sleep(retries.head)
      retry(block, accept, retries.tail)
    } else throw new RuntimeException("Retry failed")
  }
}


var test = retry[Int](
  block = () =>square(6),
  accept = res => res > 10,
  retries = List(1000, 2000, 3000)
)
