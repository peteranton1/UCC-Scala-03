package lectures.part3fp

object MapFlatMapFilterFor extends App {

  val list = List(1, 2, 3)
  println(list.head)
  println(list.tail)

  // map
  println(list.map(_ + 1))
  println(list.map(_ + " is a number"))

  // filter
  println(list.filter(_ % 2 == 0))

  // flatmap
  val toPair = (x: Int) => List(x, x + 1)
  println(list.flatMap(toPair))

  /*
  Exercise:
  print out all combinations between
  two lists
  */
  val numbers = List(1, 2, 3, 4)
  val chars = List('a', 'b', 'c', 'd')
  val colors = List("black", "white")
  val combinations = numbers
    .flatMap(n => chars
      .flatMap(c => colors
        .map(color => "" + c + n + "-" + color)))
  println(combinations)

  // forEach
  list.foreach(println)

  // for comprehensions
  val forCombinations = for {
    n <- numbers if n % 2 == 0
    c <- chars
    color <- colors
  } yield "" + c + n + "-" + color
  println(forCombinations)

  for {
    n <- numbers
  } println(n)

  // syntax overload
  val listTimesTwo = list.map { x =>
    x * 2
  }
  println(listTimesTwo)

  /*
  Exercise:
  1. see if MyList supports for comprehensions
  2. A small collection of at most ONE element
    called Maybe[+T]
    - map, flatMap, filter
  */
  import exercises.mylist6._

  val mylist1 = Cons(1,Cons(2,Cons(3, Empty)))

  val mylist1result = for {
    n <- mylist1
  } yield n * 3
  println(mylist1result)

  
}
