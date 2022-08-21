package exercises.mylist2

object Generics extends App {

  class MyList[+A] {
    // use type A
    def add[B >: A](element: B): MyList[B] = ???
    /*
    A = Cat
    B = Animal
    */
  }
  class MyMap[K, V]

  val listOfIntegers = new MyList[Int]
  val listOfStrings = new MyList[String]

  // generic methods
  object MyList {
    def empty[A]: MyList[A] = ???
  }
  val emptyListOfIntegers = MyList.empty[Int]

  // variance problem
  class Animal
  class Cat extends Animal
  class Dog extends Animal

  // 1. yes List of cat extends list of Animal
  // = COVARIANCE
  class CoVariantList[+A]
  val animal: Animal = new Cat
  val animalList: CoVariantList[Animal] =
    new CoVariantList[Cat]
  // animalList.add(new Dog) ??? HARD QUESTION
  // Answer: Return list of animals
  // def add[B >: A](element: B): MyList[B] = ???

  // 2. no
  // = INVARIANCE
  class InvariantAnimalList[A]
  val invariantAnimalList: InvariantAnimalList[Animal] =
    new InvariantAnimalList[Animal]

  // 3. hell no
  // = CONTRAVARIANCE
  class Trainer[-A]
  val trainer: Trainer[Cat] =
    new Trainer[Animal]

  // bounded types
  class Cage[A <: Animal](animal: A)
  val cage = new Cage(new Dog)

  /*
    expand mylist to be generic.
  */

}
