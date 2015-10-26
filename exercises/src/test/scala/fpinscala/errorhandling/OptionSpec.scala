package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.errorhandling.{Option, Some, None}
import fpinscala.errorhandling.Option.{ variance, map2, sequence, traverse }

class OptionSpec extends FlatSpec with Matchers {

  "map" should s"result in ${None} when given ${None}" in {
    None.map(identity).shouldBe(None)
  }
  it should "apply the function to the value contained in a Some and construct a Some" in {
    Some(1).map(_+1).shouldBe(Some(2))
  }

  "getOrElse" should s"provide the else when given a ${None}" in {
    None.getOrElse(1).shouldBe(1)
  }
  it should "provide the value of a Some when given a Some" in {
    Some("s").getOrElse("a").shouldBe("s")
  }
  "flatMap" should s"result in ${None} when given ${None}" in {
    None.flatMap(identity).shouldBe(None)
  }
  it should "return the result of applying the value to the function" in {
    Some(1).flatMap((_) => None).shouldBe(None)
  }

  "orElse" should s"return ${None} when the option being called on is ${None}" in {
    None.orElse(None).shouldBe(None)
  }
  it should "return the Options when the option is Some" in {
    Some(1).orElse(None).shouldBe(Some(1))
  }

  "filter" should s"return ${None} when the predicate is false" in {
    Some(1).filter((_)=>false).shouldBe(None)
  }
  it should "return the value constructed with Some when the predicate is true" in {
    Some(1).filter((_)=>true).shouldBe(Some(1))
  }

  "variance" should s"return ${None} when given an empty seq" in {
    variance(Seq()).shouldBe(None)
  }
  it should "return the correct result" in {
    variance(Seq(600, 470, 170, 430, 300)).shouldBe(Some(21704))
  }

  "map2" should s"return ${None} if either option is ${None}" in {
    val someFunc = (a:Int,b:Int) => a
    map2(None, Some(1))(someFunc).shouldBe(None)
    map2(Some(1), None)(someFunc).shouldBe(None)
  }
  it should s"return the result of applying f to both option values when both options are some" in {
    map2(Some(3), Some(2))((a,b)=>a+b).shouldBe(Some(5))
  }

  "sequence" should s"result in ${None} when the list contains ${None}" in {
    sequence(List(Some(1), None, Some(2))).shouldBe(None)
  }
  it should s"result in a list of the option values when the list does not contain ${None}" in {
    sequence(List(Some(1), Some(2))).shouldBe(Some(List(1,2)))
  }

  "traverse" should s"result in ${None} when the mapping function returns ${None}" in {
    traverse(List(1,2))((_)=>None).shouldBe(None)
  }
  it should s"result in a list with the mapping function applied when the mapping function does not return ${None}" in {
    traverse(List(1,2))((a)=>Some(a*2)).shouldBe(Some(List(2,4)))
  }
}
