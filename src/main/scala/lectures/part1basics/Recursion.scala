package lectures.part1basics

import scala.annotation.tailrec

object Recursion extends App {

  def factorial(n: Int): Int =
    if (n <= 1) 1
    else {
      n * factorial(n - 1)
    }

  def anotherFactorial(n: Int): Int = {
    @tailrec
    def facHelper(x: Int, acc: Int): Int =
      if (x <= 1) acc
      else facHelper(x - 1, x * acc)

    facHelper(n, 1)
  }

  //  println(s"factorial of 10 is ${factorial(10)}")
  //
  //  println(s"another factorial of 10 is " +
  //    s"${anotherFactorial(10)}")

  /*
  1. Concatenate a string n times
  2. isPrime function tail recursive
  3. fibonacci function tail recursive
  */
  //  1. Concatenate a string n times
  @tailrec
  def concatenateTailrec(aString: String, n: Int, acc: String): String = {
    if (n <= 0) acc
    else concatenateTailrec(aString, n - 1, aString + acc)
  }

  //  println(concatenateTailrec("Hi", 5, ""))

  //  2. isPrime function tail recursive

  def isPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeRec(t: Int, isStillPrime: Boolean): Boolean = {
      if (!isStillPrime) false
      else if (t <= 1) true
      else isPrimeRec(t - 1, n % t != 0 && isStillPrime)
    }

    isPrimeRec(n / 2 , true)
  }
  //  println(s"isPrime(13) = ${isPrime(13)}")
  //  println(s"isPrime(15) = ${isPrime(15)}")
    println(s"isPrime(2003) = ${isPrime(2003)}")
    println(s"isPrime(629) = ${isPrime(629)}")

  // 3. fibonacci function tail recursive
  def fibonacci(n: Int): Int = {
    @tailrec
    def fiboTailRec(i: Int, last: Int, nextToLast: Int): Int = {
      if (i >= n) last
      else fiboTailRec(i + 1, last + nextToLast, last)
    }

    if(n <= 2) 1
    else fiboTailRec(2, 1, 1)
  }

  var i = 1
  val end = 8
  while (i <= end) {
    println(s"A fibonacci($i)=${fibonacci(i)}")
    i += 1
  }
  for(i <- 1 to 8){
    println(s"B fibonacci($i)=${fibonacci(i)}")
  }
  (1 to 8).foreach{
    i => println(s"C fibonacci($i)=${fibonacci(i)}")
  }
}
