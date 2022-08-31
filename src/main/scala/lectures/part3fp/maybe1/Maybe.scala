package lectures.part3fp.maybe1

abstract class Maybe[+T] {
  def isEmpty: Boolean
  def map[B](transformer: T => B): Maybe[B]
  def flatMap[B](transformer: T => Maybe[B]): Maybe[B]
  def filter(predicate: T => Boolean): Maybe[T]
}
case object MaybeNot extends Maybe[Nothing] {
  def isEmpty: Boolean = true
  def map[B](transformer: Nothing => B): Maybe[B] =
    MaybeNot
  def flatMap[B](transformer: Nothing => Maybe[B]): Maybe[B] =
    MaybeNot
  def filter(predicate: Nothing => Boolean): Maybe[Nothing] =
    MaybeNot
}
case class Just[+T](value: T) extends Maybe[T] {
  def isEmpty: Boolean = false
  def map[B](transformer: T => B): Maybe[B] =
    Just(transformer(value))
  def flatMap[B](transformer: T => Maybe[B]): Maybe[B] =
    transformer(value)
  def filter(predicate: T => Boolean): Maybe[T] =
    if(predicate(value)) this
    else MaybeNot
}
object MaybeTest extends App {
  val m1 = Just(42)
  val m2 = Just(43)
  val m3 = MaybeNot
  val list = List(m1,m2,m3)
  println(list)
  println(list.filter(m => !m
    .filter(_ % 2 == 0)
    .isEmpty))
  println(list.map(m => m
    .map(_ * 2)))
  println(list.map(m => m
    .flatMap(v => Just(v * 2))))
  val a123 = for {
    a1 <- m1
    a2 <- m2
//    a3 <- m3
  } yield "|" + a1 +
    ":"  + a2 +
    //":"  + a3 +
    "|"
  println(a123)
}
