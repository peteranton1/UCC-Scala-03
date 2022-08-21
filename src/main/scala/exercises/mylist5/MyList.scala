package exercises.mylist5


/*
Demonstrate the use of Function types
*/

abstract class MyList[+A] {

  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyList[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"

  // Higher order functions
  def map[B](transformer: A => B): MyList[B]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]

  def ++[B >: A](list: MyList[B]): MyList[B]
}

object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing](element: B): MyList[B] =
    Cons(element, Empty)
  def printElements: String = ""
  def map[B](transformer: Nothing => B): MyList[B] =
    Empty
  def filter(predicate: Nothing => Boolean): MyList[Nothing] =
    Empty
  def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] =
    Empty

  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list
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

  def filter(predicate: A => Boolean): MyList[A] =
    if(predicate(head)) Cons(h, t.filter(predicate))
    else t.filter(predicate)

  def map[B](transformer: A => B): MyList[B] =
    Cons(transformer(h), t.map(transformer))

  def ++[B >: A](list: MyList[B]): MyList[B] =
    Cons(h, t ++ list)

  def flatMap[B](transformer: A => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)
}

object ListTest extends App {
  val listOfIntegers: MyList[Int] =
    Cons(1, Cons(2, Cons(3, Empty)))
  val anotherListOfIntegers: MyList[Int] =
    Cons(4, Cons(5, Empty))
  val listOfStrings: MyList[String] =
    Cons("Hello", Cons("Scala", Empty))
  println(listOfIntegers.toString)
  println(listOfStrings.toString)
  println(listOfIntegers.map(elem => elem * 2))
  println(listOfIntegers.filter(elem => elem % 2 == 0))
  println(listOfStrings.map(elem => elem + "X"))
  println(listOfStrings.filter(elem => elem == "Hello"))
  println(listOfIntegers ++ anotherListOfIntegers)
  println(listOfIntegers
    .flatMap(elem => Cons(elem, Cons(elem + 1, Empty))))
  println(listOfStrings
    .flatMap(elem => Cons(elem, Cons(elem + "X", Empty))))
}