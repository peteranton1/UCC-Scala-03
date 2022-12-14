package com.example

import org.scalatest.flatspec.AnyFlatSpec

class FooTest extends UnitSpec {

  "runit" should "return known string" in {
    val input = "abc"
    val expected = s"str: $input"
    val app = new Foo()
    val actual = app.runIt(input)
    assert(actual === expected)
  }

  it should "throw exception" in {
    val s = "hi"
    assertThrows[IndexOutOfBoundsException] {
      s.charAt(-1)
    }
  }

  it should "throw exception with message" in {
    val s = "hi"
    val result = intercept[IndexOutOfBoundsException] {
      s.charAt(-1)
    }.getMessage

    result should equal("Index -1 out of bounds for length 2")
  }

}
