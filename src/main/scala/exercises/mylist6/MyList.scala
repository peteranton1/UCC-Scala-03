package exercises.mylist6

import scala.annotation.tailrec


/*
Expand MyList
- foreach method A => Unit
- sort function ((A, A) => Int) MyList
- zipWith (list, (A, A) => Int) => MyList)
- fold(start)(function) = a value
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

  //- foreach method A => Unit
  def foreach(transformer: A => Unit): Unit

  def reverse(): MyList[A]

  def insertionSort[B >: A](list: MyList[B],
                            comparer: (B, B) => Int): MyList[B]

  //- sort function ((A, A) => Int) MyList
  def sort[B >: A](comparer: (B, B) => Int): MyList[B]

  //- zipWith (list, (A, A) => Int) => MyList)
  def zipWith[B >: A](other: MyList[B],
                      combiner: (B, B) => B): MyList[B]

  //- fold(start)(function) = a value
  def fold[B >: A](start: B)(combiner: (B, B) => B): B
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

  def foreach(transformer: Nothing => Unit): Unit = {}

  def insertionSort[B >: Nothing](list: MyList[B],
                                  comparer: (B, B) => Int): MyList[Nothing] =
    Empty

  def sort[B >: Nothing](comparer: (B, B) => Int): MyList[B] =
    Empty

  def reverse(): MyList[Nothing] = Empty

  def zipWith[B >: Nothing](other: MyList[B],
                            combiner: (B, B) => B): MyList[B] = Empty

  def fold[B >: Nothing](start: B)(combiner: (B, B) => B): B =
    throw new NoSuchElementException
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
    if (predicate(head)) Cons(h, t.filter(predicate))
    else t.filter(predicate)

  def map[B](transformer: A => B): MyList[B] =
    Cons(transformer(h), t.map(transformer))

  def ++[B >: A](list: MyList[B]): MyList[B] =
    Cons(h, t ++ list)

  def flatMap[B](transformer: A => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)

  def foreach(transformer: A => Unit): Unit = {
    transformer(h)
    t.foreach(transformer)
  }

  def reverse(): MyList[A] = {
    @tailrec
    def inner(inList: MyList[A], outList: MyList[A]): MyList[A] =
      if (inList.isEmpty) outList
      else inner(inList.tail, Cons(inList.head, outList))

    inner(this, Empty)
  }

  def insertionSort[B >: A](list: MyList[B],
                            comparer: (B, B) => Int): MyList[B] = {
    @tailrec
    def insertTailRec(element: B,
                      sortedList: MyList[B],
                      acc: MyList[B]): MyList[B] =
      if (sortedList.isEmpty || comparer(element, sortedList.head) >= 0)
        acc.reverse() ++ Cons(element, sortedList)
      else
        insertTailRec(element,
          sortedList.tail, Cons(sortedList.head, acc))

    @tailrec
    def sortTailRec(inList: MyList[B],
                    acc: MyList[B]): MyList[B] = {
      if (inList.isEmpty) acc
      else
        sortTailRec(inList.tail,
          insertTailRec(inList.head, acc, Empty))
    }

    sortTailRec(list, Empty)
  }

  def sort[B >: A](comparer: (B, B) => Int): MyList[B] =
    insertionSort(this, comparer)

  def zipWith[B >: A](other: MyList[B],
                      combiner: (B, B) => B): MyList[B] = {
    @tailrec
    def zipWithTailRec(listA: MyList[B],
                       listB: MyList[B],
                       acc: MyList[B]): MyList[B] = {
      if (listA.isEmpty || listB.isEmpty) acc.reverse()
      else
        zipWithTailRec(
          listA.tail,
          listB.tail,
          Cons(combiner(listA.head, listB.head), acc))
    }

    zipWithTailRec(this, other, Empty)
  }

  def fold[B >: A](start: B)(combiner: (B, B) => B): B = {
    @tailrec
    def inner(list: MyList[B], acc: B): B =
      if (list.isEmpty) acc
      else inner(list.tail, combiner(list.head, acc))

    inner(this, start)
  }

}

object ListTest extends App {
  val listOfIntegers: MyList[Int] =
    Cons(1, Cons(2, Cons(3, Empty)))
  val anotherListOfIntegers: MyList[Int] =
    Cons(4, Cons(5, Cons(6, Empty)))
  val listOfStrings: MyList[String] =
    Cons("AHello", Cons("BScala", Cons("Cat", Empty)))

  listOfIntegers
    .foreach(elem => println(s"elem = $elem"))
  listOfStrings
    .foreach(elem => println(s"elem = $elem"))

  val sortedInts = listOfIntegers
    .sort((x: Int, y: Int) => Integer.compare(x, y))
  println(s"$listOfIntegers sorted ${sortedInts}")
  val sortedStrings = listOfStrings
    .sort((x: String, y: String) => x.compare(y))
  println(s"$listOfStrings sorted ${sortedStrings}")

  val zippedInts = listOfIntegers
    .zipWith(anotherListOfIntegers,
      (x: Int, y: Int) => x * y)
  println(s"$listOfIntegers and " +
    s"$anotherListOfIntegers zipWith " +
    s"${zippedInts}")

  println(s"$listOfIntegers fold " +
    s"${listOfIntegers.fold(0)((a: Int, b: Int) => a + b)}")
}