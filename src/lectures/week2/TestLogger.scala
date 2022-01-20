package lectures.week2

object TestLogger extends App{
  class Logger(val msgNum: Int = 0) {
    def info(): Logger = {
      println("INFO New Message")
      new Logger(msgNum + 1)
    }

    def info(n:Int): Logger = {
      if (n <= 0) this
      else info.info(n - 1)
    }
    def print: Unit = println(msgNum)
  }

  val logger = new Logger
  logger.info(2).info(2).print
  logger.info.info.info.print
}
