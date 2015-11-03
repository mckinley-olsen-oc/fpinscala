package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList(): List[A] = this match {
    case Cons(xT, xsT) => List(xT()) ++ xsT().toList()
    case Empty => List()
  }

  def take(n: Int): Stream[A] = {
    if(n==0)
      Empty
    else this match {
      case Cons(xT, xsT) => Cons(xT, ()=>xsT().take(n-1))
      case Empty => Empty
    }
  }

  def drop(n: Int): Stream[A] = sys.error("todo")

  /*
  def takeWhile(p: A => Boolean): Stream[A] = 
    this match {
      case Cons(xT, xsT) if(p(xT())) => Cons(xT, ()=>xsT().takeWhile(p))
      case _ => Empty
    }
   */

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight(Empty:Stream[A])(
      (x, xsT)=>
        if(p(x))
          Cons(()=>x, ()=>xsT)
        else
          Empty
    )
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(xT, xsT) => p(xT()) && xsT().forAll(p)
    case Empty => true
  }

  def headOption: Option[A] = {
    this.foldRight(None:Option[A])((x, xsT)=>Some(x))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A=>B) = {
    foldRight(Empty:Stream[B])(
      (x, xsT) => Cons(()=>f(x), ()=>xsT)
    )
  }
  def filter(f:A=>Boolean) ={
    foldRight(Empty:Stream[A])(
      (x, xsT)=>
      if(f(x))
        Cons(()=>x, ()=>xsT)
      else
        xsT
    )
  }
  def append[B>:A](app:Stream[B]):Stream[B] ={
    foldRight(app:Stream[B])(
      (x, xsT) => {
        Cons(()=>x, ()=>xsT)
      }
    )
  }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}
