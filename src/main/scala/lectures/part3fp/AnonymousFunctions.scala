package lectures.part3fp

object AnonymousFunctions extends App {

  // Anonymous Functions LAMBDA
  val doubler: Int => Int = x => x * 2

  // multiple params in a lambda
  val adder: (Int, Int) => Int =
    (a: Int, b: Int) => a + b

  val justDoSomething: () => Int = () => 3

  println(justDoSomething)
  println(justDoSomething())

  // curly braces
  val stringToInt = { (str: String) =>
    str.toInt
  }

  // MORE sugar
  val niceIncrementer: Int => Int = _ + 1
  val niceAdder: (Int, Int) => Int = _ + _

  // lambdas of prev func
  val superAdd = (x: Int) => (y: Int) => x + y
  println(superAdd(3)(4))
}
