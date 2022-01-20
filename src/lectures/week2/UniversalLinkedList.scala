package lectures.week2

object UniversalLinkedList extends App {
  abstract class LogList[+A] {
    def add[B >: A](msg: B): LogList[B]

    def last: A

    def previous: LogList[A]

    def all: String

    def isEmpty: Boolean
  }

  class Log[+A](head: A, tail: LogList[A]) extends LogList[A] {
    def add[B >: A](msg: B): LogList[B] = new Log[B](msg, this)

    def last: A = head

    def previous: LogList[A] = tail

    def all: String = head.toString + " " + tail.all

    def isEmpty: Boolean = if (head.toString.isEmpty || head == null) true else false
  }

  object Empty extends LogList[Nothing] {
    def add[B](msg: B): LogList[B] = new Log[B](msg, Empty)

    def last = throw new NoSuchElementException

    def previous = throw new NoSuchElementException

    def all: String = ""

    def isEmpty: Boolean = true
  }
}
