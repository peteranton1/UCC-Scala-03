package exercises.mylist1

abstract class MyList {

  /*
  head = first element of the list
  tail = remainder of list
  isEmpty = is the list empty
  add(int) => new list with element added
  toString => returns string representation of the list.
  */

  def head: Int
  def tail: MyList
  def isEmpty: Boolean
  def add(element: Int): MyList
  def printElements: String
  override def toString: String = "[" + printElements + "]"
}

object Empty extends MyList {
  def head: Int = throw new NoSuchElementException
  def tail: MyList = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add(element: Int): MyList =
    Cons(element, Empty)
  def printElements: String = ""
}

case class Cons(h: Int,
                t: MyList) extends MyList {
  def head: Int = h
  def tail: MyList = t
  def isEmpty: Boolean = false
  def add(element: Int): MyList =
    Cons(element, this)

  def printElements: String =
    if (t.isEmpty) "" + h
    else h + " " + t.printElements
}

object ListTest extends App {
  val list = Cons(1, Cons(2, Cons(3, Empty)))
  println(list.tail.head)
  println(list.add(4).head)
  println(list.isEmpty)
  println(list.toString)
}