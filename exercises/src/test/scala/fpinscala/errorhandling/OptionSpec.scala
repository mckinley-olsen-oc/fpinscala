package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.errorhandling.{Option, Some, None}

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

}
