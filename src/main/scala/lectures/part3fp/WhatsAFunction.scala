package lectures.part3fp

object WhatsAFunction extends App {

  // Use and work with functions as
  // first class elements
  // problem: oop

  val doubler = new MyFunction[Int, Int] {
    override def apply(element: Int): Int = element * 2
  }

  println(doubler(2))
}

trait MyFunction[A, B] {
  def apply(element: A): B
}
