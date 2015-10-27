package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(right) => Right(f(right))
    case Left(left) => Left(left)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(right) => f(right)
    case Left(left) => Left(left)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(right) => Right(right)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      valueA <- this
      valueB <- b
    } yield f(valueA, valueB)
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    def helper(acc:Either[E, List[B]], value:A):Either[E, List[B]] = acc match {
      case Right(l) => f(value) match {
        case Right(result) => Right(l :+ result)
        case Left(err) => Left(err)
      }
      case Left(err) => Left(err)
    }
    es.foldLeft(Right(List[B]()):Either[E, List[B]])(helper)
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = {
    traverse(es)(identity)
  }

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}