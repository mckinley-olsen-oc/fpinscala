package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.datastructures.List.{tail}

class ListSpec extends FlatSpec with Matchers {

  "tail" should s"return ${Nil} when the list is Nil" in {
    tail(Nil) should be(Nil)
  }
  it should s"return ${Nil} (the tail) when given a list with one element" in {
    tail(Cons[Int](2, Nil)) should be(Nil)
  }
  it should "return the list without the head when given a list of two elements" in {
    val listTail = Cons[Int](3, Nil)
    tail(Cons[Int](2, listTail)) should be (listTail)
  }

}
