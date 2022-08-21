package exercises.mylist3

object AnonymousClasses extends App {

  abstract class Animal {
    def eat(): Unit
  }

  // anonymous class
  val funnyAnimal: Animal = new Animal {
    override def eat(): Unit = println("hahaha")
  }

  funnyAnimal.eat()
  println(funnyAnimal.getClass)

  class Person(name: String) {
    def sayHi: String = s"Hi I am $name"
  }
  val jim = new Person("Jim") {
    override def sayHi: String =
      s"Jim here: ${super.sayHi}"
  }
  println(jim.sayHi)
}
