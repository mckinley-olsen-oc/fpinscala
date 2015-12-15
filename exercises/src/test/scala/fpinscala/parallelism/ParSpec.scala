package fpinscala.parallelism

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.parallelism.Par._

import java.util.concurrent.{ ExecutorService, Executors }

class ParSpec extends FlatSpec with Matchers {
  val poolSize=1
  val q = Executors.newFixedThreadPool(poolSize)
  "sequence" should "take a list of pars and produce a par of list containing what those pars evalutate to" in {
    val testval = 99
    val exlist = List(unit(testval))
    sequence(exlist)(q).get().head.shouldBe(testval)
    sequence2(exlist)(q).get().head.shouldBe(testval)
  }

  "parFilter" should "produce an empty list with a predicate that always returns false" in {
    val exList = List(99)
    parFilter(exList)(_=>false)(q).get.shouldBe(List())
  }
}
