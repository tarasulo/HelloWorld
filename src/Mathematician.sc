

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
    if (!flag && n == m) return
    if (flag) {
      if (m - 4 > 0) tailFunc(n, m - 4, flag =true)
      else tailFunc(n, m - 4, flag = false)
    } else tailFunc(n, m + 4, flag = false)
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

def retry[T](block: () => T,
             accept: T => Boolean,
             retries: List[Long]): T = {
  val count = retries(0)
  var pause = retries(1)
  try {
    val result = block()
    if (accept(result)) {
      println("Retry success " + result)
      return result
    }
    else {
      if(count >0) {
        Thread.sleep(pause)
        if (retries.size == 3) pause = retries(2)
        retry(block, accept, List(count-1, pause))
      } else throw new RuntimeException("Retry failed")
    }
  } catch {
    case e: Exception => throw new Exception("Retry aborted \n" + e )
  }
}


var test = retry[Int](
  block = () =>square(6),
  accept = res => res > 10,
  retries = List(2, 1000, 2000)
)



