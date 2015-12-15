package fpinscala.monoids

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.monoids.Monoid._

class MonoidSpec extends FlatSpec with Matchers {

  "intAddition" should "have a zero condition of 0" in {
    intAddition.zero.shouldBe(0)
  }
  it should "correctly add two numbers" in {
    intAddition.op(1,2).shouldBe(3)
  }

  "intMultiplication" should "have a zero condition of 1" in {
    intMultiplication.zero.shouldBe(1)
  }
  it should "correctly multiple two numbers" in {
    intMultiplication.op(10, 10).shouldBe(100)
  }

  "booleanOr" should "have a zero condition of false" in {
    booleanOr.zero.shouldBe(false)
  }
  it should "correctly or two boolean values" in {
    booleanOr.op(true, false).shouldBe(true)
    booleanOr.op(false,false).shouldBe(false)
  }
}
