package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth(t: Tree[Int]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (1 + depth(l)) max (1 + depth(r))
  }

  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l, f), map(r, f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeFold[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)
  def maxFold(t: Tree[Int]): Int = fold(t)(x => x)(_ max _ )
  def depthFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((x, y) => (x + 1) max (y + 1))

}