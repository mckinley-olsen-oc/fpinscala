package fpinscala.state

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.state.RNG.{Simple, nonNegativeInt, double, ints, intDouble, randIntDouble, map, flatMap, mapFlatMap, map2, map2FlatMap, boolean }
import fpinscala.state.State

class StateSpec extends FlatSpec with Matchers {

  val m = Map(-67->(-25778177))
  val mNext = Map(-67 -> 279785578148228L)

  "nonNegativeInt" should "convert a negative integer to a positive" in {
    val int = -67
    nonNegativeInt(Simple(int))._1.shouldBe(-(m.get(int).get))
  }

  "double" should "produce a value between 0 and 1" in {
    val dbl = double(Simple(67))._1
    assert(dbl < 1 && dbl > 0)
  }

  "boolean" should "produce a boolean value" in {
    boolean(Simple(m.keySet.head)).shouldBe((false, Simple(mNext.get(m.keySet.head).get)))
  }

  "ints" should "return an empty list when given a count of 0" in {
    ints(0)(Simple(67)).shouldBe((List(), Simple(67)))
  }
  it should "return a single element list with when given a count of 1" in {
    val int = -67
    ints(1)(Simple(int)).shouldBe((List(m.get(int).get), Simple(mNext.get(int).get)))
  }

  "intDouble" should "produce a random ((int, double), RNG)" in {
    val int = -67
    intDouble(Simple(-67)).shouldBe(((m.get(int).get, 0.05228794135725495), Simple(7358873546047L)))
  }

  "randIntDouble" should "produce a random ((int, double), RNG)" in {
    val int = -67
    randIntDouble(Simple(-67)).shouldBe(((m.get(int).get, 0.05228794135725495), Simple(7358873546047L)))
  }

  "map" should "take"+
    "\n1. a function dependent on state that produces a value and next state"+
    "\nand"+
    "\n2. a function that takes the value produced by the first function and returns a new value"+
    "\nand return a function that takes a state, provide the state to the first function and provides the value"+
    "produced to the 2nd function, then returns the value produced by the 2nd function along with the new state"+
    "produced by the first function" in {
      val int = -67
      map(nonNegativeInt)(_+1)(Simple(int)).shouldBe((-(m.get(int).get)+1, Simple(mNext.get(int).get)))
  }
  "mapFlatMap" should "take"+
    "\n1. a function dependent on state that produces a value and next state"+
    "\nand"+
    "\n2. a function that takes the value produced by the first function and returns a new value"+
    "\nand return a function that takes a state, provide the state to the first function and provides the value"+
    "produced to the 2nd function, then returns the value produced by the 2nd function along with the new state"+
    "produced by the first function" in {
      val int = -67
      mapFlatMap(nonNegativeInt)(_+1)(Simple(int)).shouldBe((-(m.get(int).get)+1, Simple(mNext.get(int).get)))
    }

  "map2" should "take"+
    "\n1. two functions dependent on state that produce a value and next state"+
    "\nand"+
    "\n2. a function that takes the values produced by the first functions and returns a new value"+
    "\nand return a function that takes a state, provides the state to the first of the first functions,"+
    "provide the state produced to the 2nd of the first functions, and provide the values"+
    "produced to the 2nd function, then returns the value produced by the 2nd function along with"+
    "the state produced by the 2nd of the first functions" in {
      val endStateSeed = 76
      val f1Val=24
      val f2Val=54
      map2((_)=>(f1Val,Simple(234)), (_)=>(f2Val, Simple(endStateSeed)))(_+_)(Simple(65))
        .shouldBe(f1Val+f2Val, Simple(endStateSeed))
    }
  "map2FlatMap" should "take"+
    "\n1. two functions dependent on state that produce a value and next state"+
    "\nand"+
    "\n2. a function that takes the values produced by the first functions and returns a new value"+
    "\nand return a function that takes a state, provides the state to the first of the first functions,"+
    "provide the state produced to the 2nd of the first functions, and provide the values"+
    "produced to the 2nd function, then returns the value produced by the 2nd function along with"+
    "the state produced by the 2nd of the first functions" in {
      val endStateSeed = 76
      val f1Val=24
      val f2Val=54
      map2FlatMap((_)=>(f1Val,Simple(234)), (_)=>(f2Val, Simple(endStateSeed)))(_+_)(Simple(65))
        .shouldBe(f1Val+f2Val, Simple(endStateSeed))
    }

  "flatMap" should "take"+
    "\n1. a function dependent on state that produces a value and next state"+
    "\nand"+
    "\n2. a function that takes the value produced by the first function, returning a function that takes"+
    "the state returned from the first function and returns a new value and next state"+
    "\nand return a function that takes a state, provides the state to the first function and provides the value"+
    "produced to the 2nd function, then provides the state produced by the first function to the function"+
    "returned by the 2nd function" in {
      val endStateSeed = 76
      val int = -67
      flatMap(nonNegativeInt)((i)=>(nextState)=>(i, Simple(endStateSeed)))(Simple(int))
        .shouldBe((-( m.get(int).get), Simple(endStateSeed)))
  }

  "State.map" should "go from one state to the next" in {
    val int = -67
    State(nonNegativeInt).map(_+1).run(Simple(int)).shouldBe((-(m.get(int).get) +1), Simple(279785578148228L))
  }

}
