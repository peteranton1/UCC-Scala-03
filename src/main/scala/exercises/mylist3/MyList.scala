package exercises.mylist3

trait MyPredicate[-T] {
  def test(t: T): Boolean
}
trait MyTransformer[-A, B] {
  def transform(a: A): B
}
/*
MyList:
- map(transformer) => MyList
- filter(predicate) => MyList
- flatMap(transformer from A to MyList[B]) => MyList[B]

class EvenPredicate extends MyPredicate[Int]
class StringToIntTransformer extends MyTransformer[String, Int]

[1,2,3].map(n + 2) = [2,4,6]
[1,2,3,4].filter(n % 2) = [2,4]
[1,2,3].flatMap(n => [n, n+1]) => [1,2,2,3,3,3,4]
*/

abstract class MyList[+A] {

  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyList[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"
  /*
    - map(transformer) => MyList
    - filter(predicate) => MyList
    - flatMap(transformer from A to MyList[B]) => MyList[B]
  */
  def map[B](t: MyTransformer[A, B]): MyList[B]
  def flatMap[B](t: MyTransformer[A, MyList[B]]): MyList[B]
  def filter(p: MyPredicate[A]): MyList[A]

  def ++[B >: A](list: MyList[B]): MyList[B]
}

object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing](element: B): MyList[B] =
    Cons(element, Empty)
  def printElements: String = ""
  def map[B](t: MyTransformer[Nothing, B]): MyList[B] =
    Empty
  def filter(p: MyPredicate[Nothing]): MyList[Nothing] =
    Empty
  def flatMap[B](t: MyTransformer[Nothing, MyList[B]]): MyList[B] =
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

  def filter(p: MyPredicate[A]): MyList[A] =
    if(p.test(head)) Cons(h, t.filter(p))
    else t.filter(p)

  def map[B](transformer: MyTransformer[A, B]): MyList[B] =
    Cons(transformer.transform(h), t.map(transformer))

  def ++[B >: A](list: MyList[B]): MyList[B] =
    Cons(h, tail ++ list)

  def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] =
    transformer.transform(h) ++ t.flatMap(transformer)
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
  println(listOfStrings.map(elem => elem + elem))
  println(listOfStrings.filter(elem => elem == "Hello"))
  println(listOfIntegers ++ anotherListOfIntegers)
  println(listOfIntegers
    .flatMap(elem => Cons(elem, Cons(elem + 1, Empty))))
}
