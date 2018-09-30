package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => Nil
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = {
    if (n == 0) return empty
    else this match {
      case Empty => empty
      case Cons(h, t) => cons(h(), t().take(n - 1))
    }
  }

  def drop(n: Int): Stream[A] = {
    if (n == 0) return this
    else this match {
      case Empty => empty
      case Cons(_, t) => t().drop(n - 1)
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      val hh = h()
      if(p(hh)) cons(hh, t().takeWhile(p))
      else empty
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) =>
      if(p(h())) t().forAll(p)
      else false
    case _ => true
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, stream) => {
    if(p(a)) cons(a, stream)
    else stream
  })

  def headOption: Option[A] = foldRight(Option.empty[A])((a, _) => { Some(a) })

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, stream) => cons(f(a), stream))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, stream) =>
    if(f(a)) cons(a, stream)
    else stream
  )

  //def append(x: A): Stream[A] = foldRight(Stream(x))((a, stream) => cons(a, stream))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((n, this)) {
    case (i, Cons(h, t)) if i > 0 => Some((h(), (i - 1, t())))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case ((Cons(h1, t1), Cons(h2, t2))) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case ((Cons(h1, t1), Empty)) => Some((Some(h1()), None), (t1(), Empty))
    case ((Empty, Cons(h2, t2))) => Some((None, Some(h2())), (Empty, t2()))
    case _ => None
  }

  def startsWith[A](s: Stream[A]): Boolean = this.zipAll(s).filter(p => p._1 != p._2) match {
    case Cons(h, _) =>
      val (h1, h2) = h()
      h1.isDefined && h2.isEmpty
    case Empty => true
  }

  def tails(): Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case s@Cons(_, t) => Some(s, t())
  }
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

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def nextFibs(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a, nextFibs(b, a + b))
    }
    nextFibs(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, newZ)) => Stream.cons(h, unfold(newZ)(f))
    case None => Stream.empty
  }

  val onesViaUnfold: Stream[Int] = unfold(None)(_ => Some(1, None))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(None)(_ => Some(a, None))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(i => Some(i, i + 1))

  def fibsViaUnfold(): Stream[Int] = unfold((0, 1))(p => Some(p._1, (p._2, p._1 + p._2)))
}