package fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.laziness.Stream.{ cons, empty }


class StreamSpec extends FlatSpec with Matchers {
  val firstElement=1
  val secondElement=2
  val thirdElement=3
  val exampleList = List(firstElement, secondElement, thirdElement)
  val exampleStream =
    Cons(()=>firstElement, ()=>Cons(()=>secondElement, ()=>Cons(()=>thirdElement, ()=>Empty)))

  "toList" should "return an empty list when given an empty stream" in {
    Empty.toList().shouldBe(List())
  }
  it should "return a list with all thunks in the stream evaluated" in {
    exampleStream
      .toList()
      .shouldBe(exampleList)
  }

  "take" should s"return ${Empty} when taking 0" in {
    Empty.take(0).shouldBe(Empty)
  }
  it should "return the entire stream when taking more than the stream's length" in {
    exampleStream.take(exampleStream.toList().length+1).toList().shouldBe(exampleList)
  }
  val howManyToTake=2
  it should s"return a stream with only the first ${howManyToTake} thunks" in {
    exampleStream.take(howManyToTake).toList().shouldBe(exampleList.take(howManyToTake))
  }
}
