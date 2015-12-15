package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Boolean
  def &&(p: Prop): Boolean = {
    this.check && p.check
  }
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val r = (stopExclusive - start)
    Gen(
      State(
          RNG.nonNegativeLessThan(r)
      ).map(i=>i+start)
    )
  }

  def Unit[A](a: => A):Gen[A] ={
    Gen(
      State(
        RNG.unit(a)
      )
    )
  }

  def boolean: Gen[Boolean] ={
    Gen(
      State(
        rng => RNG.boolean(rng)
      )
    )
  }
}

/*
trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}
 */

case class Gen[A](sample: State[RNG, A]) {

}

trait SGen[+A] {

}

