package fpinscala.errorhandling


import fpinscala.errorhandling.Option.{sequence, traverse}

import scala.{Either => _, Left => _, Option => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(r) => Right(f(r))
   case Left(l) => Left(l)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this.map(f) match {
   case Right(r) => r
   case Left(l) => Left(l)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_) => b
   case Right(r) => Right(r)
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap(aa => b.map(bb => f(aa, bb)))
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => traverse(t)(f).flatMap(tt => f(h).map(hh => hh :: tt))  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = es match {
    case Nil => Right(Nil)
    case Left(l) :: _ => Left(l)
    case Right(r) :: t => sequence(t).map(r :: _)
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