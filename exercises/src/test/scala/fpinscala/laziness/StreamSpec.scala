package fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.laziness.Stream.{ cons, empty }
import fpinscala.laziness.Stream.{constant, from}


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

  "headOption" should s"return ${None} when called on an empty stream" in {
    Empty.headOption.shouldBe(None)
  }
  it should "return a Some containing the first element in the stream" in {
    exampleStream.headOption.shouldBe(Some(firstElement))
  }

  "map" should "apply the function to the elements in the stream" in {
    val mapper = (x:Int)=>x+1
    exampleStream.map(mapper).toList().shouldBe(exampleList.map(mapper))
  }
  "filter" should "filter out odds in the stream" in {
    val fil = (x:Int)=> x%2==0
    exampleStream.filter(fil).toList().shouldBe(exampleList.filter(fil))
  }
  "append" should "append to the end of the stream" in {
    val ele=1
    exampleStream.append(exampleStream).toList().shouldBe(exampleList++ exampleList)
  }

  "constant" should "produce a stream with all the same element" in {
    val ele = "a"
    assert(constant(ele).take(10).forAll((a)=>a==ele))
  }
  "from" should "produce a stream starting from that increases by one" in {
    val a = List.range(3, 13, 1)
    from(3).take(10).toList().shouldBe(a)
  }
}
