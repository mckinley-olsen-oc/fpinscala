package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.datastructures.List.{ tail, setHead, head, drop, dropWhile, init}

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

  "setHead" should "return a list with the head element being the passed value and the tail being the passed list" in {
    val testList = Cons(2, Nil)
    setHead(tail(testList), head(testList)) should be(testList)
  }

  "drop" should "return the list when dropping 0 elements" in {
    drop(Nil, 0) should be(Nil)
  }
  it should "return the list when dropping a negative number of elements" in {
    drop(Nil, -1) should be(Nil)
  }
  it should "return the tail of the list when dropping 1 element" in {
    val testList = Cons(1, Cons(2, Nil))
    drop(testList, 1) should be(Cons(2, Nil))
  }
  it should "return the tail of the tail of the tail of the list when dropping 3 elements" in {
    val testList = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    drop(testList, 3) should be(Cons(4, Nil))
  }

  "dropWhile" should "return the list when the predicate is false" in {
    val testList = Cons(1, Nil)
    dropWhile(testList, (_:Int)=>false) should be(testList)
  }
  it should "return empty list when the predicate is true" in {
    dropWhile(Cons(2, Cons(3, Nil)), (_:Int)=>true) should be(Nil)
  }

  "init" should "return the empty list when given an empty list" in {
    init(Nil) should be(Nil)
  }
  it should "return the list with all but the last element" in {
    init(Cons(2, Nil)) should be(Nil)
  }

}
