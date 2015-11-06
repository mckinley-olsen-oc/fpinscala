package fpinscala.state

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.state.RNG.{Simple, nonNegativeInt, double, ints}

class StateSpec extends FlatSpec with Matchers {

  "nonNegativeInt" should "convert a negative integer to a positive" in {
    nonNegativeInt(Simple(-67))._1.shouldBe(25778177)
  }
  "double" should "" in {
    val dbl = double(Simple(67))._1
    assert(dbl < 1 && dbl > 0)
  }
  "ints" should "return an empty list when given a count of 0" in {
    ints(0)(Simple(67)).shouldBe(List())
  }

}
