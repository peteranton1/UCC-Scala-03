package exercises.mylist2

abstract class MyList[+A] {

  /*
  head = first element of the list
  tail = remainder of list
  isEmpty = is the list empty
  add(int) => new list with element added
  toString => returns string representation of the list.
  */

  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyList[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"
}

object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing](element: B): MyList[B] =
    Cons(element, Empty)
  def printElements: String = ""
}

case class Cons[+A](h: A,
                t: MyList[A]) extends MyList[A] {
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): MyList[B] =
    Cons(element, this)

  def printElements: String =
    if (t.isEmpty) "" + h
    else h.toString + " " + t.printElements
}

object ListTest extends App {
  val listOfIntegers: MyList[Int] =
    Cons(1, Cons(2, Cons(3, Empty)))
  val listOfStrings: MyList[String] =
    Cons("Hello", Cons("Scala", Empty))
  println(listOfIntegers.tail.head)
  println(listOfIntegers.add(4))
  println(listOfIntegers.isEmpty)
  println(listOfIntegers.toString)
  println(listOfStrings.tail.head)
  println(listOfStrings.add("!"))
  println(listOfStrings.isEmpty)
  println(listOfStrings.toString)
}
