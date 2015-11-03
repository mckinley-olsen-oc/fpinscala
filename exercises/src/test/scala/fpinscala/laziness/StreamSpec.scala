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
    exampleStream.take(0).shouldBe(Empty)
  }
  it should "return the entire stream when taking more than the stream's length" in {
    exampleStream.take(exampleStream.toList().length+1).toList().shouldBe(exampleList)
  }
  val howManyToTake=2
  it should s"return a stream with only the first ${howManyToTake} thunks" in {
    exampleStream.take(howManyToTake).toList().shouldBe(exampleList.take(howManyToTake))
  }

  "takeWhile" should s"return ${Empty} when given a constantly false predicate" in {
    exampleStream.takeWhile((_)=>false).shouldBe(Empty)
  }
  val howManyToTakeWhile = 2
  it should s"return the first ${howManyToTakeWhile}" in {
    exampleStream.takeWhile((el)=> el != exampleList.drop(howManyToTakeWhile).head)
      .toList()
      .shouldBe(exampleList.take(howManyToTakeWhile))
  }

  "forAll" should "terminate at the first false" in {
    var earlyTerminated = true
    exampleStream.forAll((ele)=>if(ele>=secondElement){earlyTerminated=false;false}else false)
      .shouldBe(false)
    assert(earlyTerminated)
  }
  it should s"return true for an ${Empty} stream" in {
    Empty.forAll((_)=>false).shouldBe(true)
  }
}
