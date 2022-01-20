package playground

object SimpleLinkedList extends App {

  abstract class LogList {
    def add(msg: String): LogList

    def last: String

    def previous: LogList

    def all: String

    def isEmpty: Boolean
  }

  class Log(head: String, tail: LogList) extends LogList {
    def add(msg: String): LogList = new Log(msg, this)

    def last: String = head

    def previous: LogList = tail

    def all: String = head + " " + tail.all

    def isEmpty: Boolean = if(head.isEmpty) true else false
  }

  object Empty extends LogList {
    def add(msg: String): LogList = new Log(msg, Empty)

    def last: String = throw new NoSuchElementException

    def previous: LogList = throw new NoSuchElementException

    def all: String = ""

    def isEmpty: Boolean = true
  }

  val list = new Log("a", new Log("b", new Log("c", Empty)))
  println(list.previous.last)

  println(list.all)

  println(list.isEmpty)
  val list2 = new Log("", Empty)
  println(list2.isEmpty)
}
