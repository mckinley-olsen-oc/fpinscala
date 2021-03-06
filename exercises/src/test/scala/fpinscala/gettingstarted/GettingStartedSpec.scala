package fpinscala.gettingstarted

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.gettingstarted.PolymorphicFunctions.{ isSorted, curry, uncurry, compose}
import fpinscala.gettingstarted.MyModule.fib

class GettingStartedSpec extends FlatSpec with Matchers {
  val intCompare = (a: Int, b: Int) => a > b

  "fib" should "return 0 for the 0th fib" in {
    fib(0) should be(0)
  }
  it should "return 1 for the 1st fib" in {
    fib(1) should be(1)
  }
  it should "return 1 for the 2nd fib" in {
    fib(2) should be(1)
  }
  it should "return 2 for the 3rd fib" in {
    fib(3) should be(2)
  }
  it should "return 3 for the 4th fib" in {
    fib(4) should be(3)
  }
  it should "return 5 for the 5th fib" in {
    fib(5) should be(5)
  }
  it should "return 8 for the 6th fib" in {
    fib(6) should be(8)
  }

  "isSorted" should "return true when given an empty array" in {
    isSorted[Int](Array(), intCompare) should be(true)
  }
  it should "return true when given a 1 element array" in {
    isSorted[Int](Array(1), intCompare) should be(true)
  }
  it should "return true when given a sorted 2 element array" in {
    isSorted[Int](Array(1,2), intCompare) should be(true)
  }
  it should "return false when given a unsorted 2 element array" in {
    isSorted[Int](Array(2,1), intCompare) should be(false)
  }

  "curry" should "return a function that takes another argument that then calls the provided function" in {
    curry[Int, Int, Int]((a,b)=>b)(1)(2) should be(2)
  }

  "uncurry" should "return a function that takes two arguments and applies them to the provided function" in {
    uncurry[Int,Int,Int]((a)=>(b)=>a)(1,2) should be(1)
  }

  "compose" should "return a function that takes the return value of the second function and provides it " +
    "to the first" in {
    compose[Int,Int,Int]((b)=>b+2, (a)=>a+1)(1) should be(4)
  }
}
