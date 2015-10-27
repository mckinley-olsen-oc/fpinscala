package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.errorhandling.{Either, Left, Right}
import fpinscala.errorhandling.Either.{ sequence, traverse }

class EitherSpec extends FlatSpec with Matchers {
  "map" should "return a left when given a left" in {
    Left(1).map(identity).shouldBe(Left(1))
  }
  it should "apply the function to the value when a right" in {
    Right(1).map(_+1).shouldBe(Right(2))
  }

  "flatMap" should "return a Left when given a Left" in {
    Left(1).flatMap(identity).shouldBe(Left(1))
  }
  it should "return what the provided function returns when given a Right" in {
    Right(1).flatMap((_)=>Right(2)).shouldBe(Right(2))
  }

  "orElse" should "return the other when the provided Either is Left" in {
    Left(1).orElse(Left(2)).shouldBe(Left(2))
  }
  it should "return the Either when the Either is Right" in {
    Right(2).orElse(Left(1)).shouldBe(Right(2))
  }

  "map2" should s"return Left if either is Left" in {
    val someFunc = (a:Int,b:Int) => a
    Left(1).map2(Right(1))(someFunc).shouldBe(Left(1))
    Right(1).map2(Left(2))(someFunc).shouldBe(Left(2))
    Left(1).map2(Left(2))(someFunc).shouldBe(Left(1))
  }
  it should s"return the result of applying f to both option values when both options are some" in {
    Right(1).map2(Right(3))((a,b)=>a+b).shouldBe(Right(4))
  }

  "sequence" should s"result in Left when the list contains Left" in {
    sequence(List(Left(1), Right(2), Right(2))).shouldBe(Left(1))
  }
  it should s"result in a list of the option values when the list does not contain ${None}" in {
    sequence(List(Right(1), Right(2))).shouldBe(Right(List(1,2)))
  }

  "traverse" should s"result in the first Left encountered when the mapping function returns Left" in {
    traverse(List(1,2,3))((value)=>Left(value)).shouldBe(Left(1))
  }
  it should s"result in a list with the mapping function applied when the mapping function does not return ${None}" in {
    traverse(List(1,2))((a)=>Right(a*2)).shouldBe(Right(List(2,4)))
  }
}
