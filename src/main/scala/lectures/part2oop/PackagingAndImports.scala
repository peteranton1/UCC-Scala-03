package lectures.part2oop

import playground.{
  PrinceCharming,
  Cinderella => Princess
}
object PackagingAndImports extends App {

  val writer = Writer("daniel", "int", 2003)

  // package objects
  sayHello()
  println(SPEED_OF_LIGHT)

  val prince = new PrinceCharming
  val princess = new Princess

  // default imports
  // java.lang, Scala, scala.Predef
}
