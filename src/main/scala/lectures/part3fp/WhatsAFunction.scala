package lectures.part3fp

import scala.annotation.tailrec

object WhatsAFunction extends App {

  // Use and work with functions as
  // first class elements
  // problem: oop

  val doubler = new MyFunction[Int, Int] {
    override def apply(element: Int): Int = element * 2
  }

  println(doubler(2))
  // function types Function1[A, B]
  val stringToIntConverter = new Function1[String, Int] {
    override def apply(v1: String): Int = v1.toInt
  }
  println(stringToIntConverter("3") + 4)

  val adder: ((Int, Int) => Int) = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }

  // Function types Function2[A, B, R] === (A, B) => R

  // ALL SCALA FUNCTIONS ARE OBJECTS

  /*
  1. function which takes two strings and concatenates them
  2. go to mylist and transform mypredicate and
  mytransformer into function types
  3. define a function which takes an int and
  returns another function which takes an int
  and returns an int.
   */
  val constructor: (String, String) => String =
    (v1: String, v2: String) => v1 + v2

  println(s"constructor(Hello,Scala) = " +
    s"${constructor("Hello","Scala")}")

  def intToInt(a: Int) : Int => Int = {
    def intToIntB(b: Int) : Int = {
      a + b
    }
    intToIntB
  }
  println(s"intToInt(3)(7) = ${intToInt(3)(7)}")

//  val superAdder: Function1[Int, Function1[Int, Int]] =
//    new Function1[Int, Function1[Int, Int]] {
//      override def apply(x: Int): Function1[Int, Int] = {
//        new Function1[Int, Int] {
//          override def apply(y: Int): Int = x + y
//        }
//      }
//    }
  val superAdder: Int => Int => Int =
    (x: Int) => (y: Int) => x + y

  val adder3 = superAdder(3)
  println(superAdder(3)(4)) // curried function
  println(adder3(4))

}

trait MyFunction[A, B] {
  def apply(element: A): B
}
