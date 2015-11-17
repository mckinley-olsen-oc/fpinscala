package fpinscala.parallelism

import org.scalatest.{FlatSpec, Matchers}
import fpinscala.parallelism.Par.{ unit, sequence }

import java.util.concurrent.{ ExecutorService, Executors }

class ParSpec extends FlatSpec with Matchers {
  val poolSize=1
  val q = Executors.newFixedThreadPool(poolSize)
  "sequence" should "take a list of pars and produce a par of list containing what those pars evalutate to" in {
    val testVal = 99
    val exList = List(unit(testVal))
    sequence(exList)(q).get().head.shouldBe(testVal)
  }
}
