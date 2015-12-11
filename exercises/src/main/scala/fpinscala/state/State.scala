package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    var (nInt, nRng) = rng.nextInt
    if(nInt == Integer.MIN_VALUE)
      nonNegativeInt(nRng)
    else if(nInt < 0)
      nInt = -nInt
    (nInt, nRng)
  }

  def boolean(rng:RNG):(Boolean, RNG) ={
    val (i, rng2) = rng.nextInt
    (i%2==0, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(_.toDouble / Integer.MAX_VALUE)(rng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i,d), r2)
  }

  def randIntDouble: Rand[(Int,Double)] = {
    both(int, double)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]) = map2(ra,rb)((_, _))

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)((_:RNG).nextInt))(rng)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      def helper(f: Rand[A], t: (List[A],RNG)) = {
        val (v, rn) = f(t._2)
        (t._1:+v, rn)
      }
      fs.foldRight((List[A](), rng))(helper)
    }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
   rng => {
   val (a, rng2) = s(rng)
   (f(a), rng2)
   }

  def mapFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)((i)=>{
                 unit(f(i))
               })

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
   rng => {
   val (a, rng1) = ra(rng)
   val (b, rng2) = rb(rng1)
   (f(a,b), rng2)
   }

  def map2FlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)((a) => {
                  (rng) => {
                    val (b, rng2) = rb(rng)
                    (f(a,b), rng2)
                  }
                })

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int):Rand[Int] =
    flatMap(nonNegativeInt)( (i) => {
                            val mod = i % n
                              if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
                            })
}
import State._
case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s1 => {
      val (a, s2) = run(s1)
      (f(a), s2)
    })


  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] = 
    sys.error("todo")

  //copied
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
