package DataStructures

import scala.annotation.tailrec

enum List[+A] {
  case Nil extends List[Nothing]
  private case Cons(h: A, t: List[A]) extends List[A]

  def tail: List[A] = this match
    case Nil        => throw new RuntimeException("Tail of empty list.")
    case Cons(h, t) => t

  def setHead[AA >: A](a: AA): List[AA] = this match
    case Nil => throw new RuntimeException("Cannot set head of empty list.")
    case Cons(_, t) => Cons(a, t)

  def init: List[A] = this match
    case Nil          => throw new RuntimeException("Init of empty list.")
    case Cons(h, Nil) => Nil
    case Cons(h, t)   => Cons(h, t.init)

  def reverse: List[A] =
    foldLeft(Nil: List[A])((b, a) => Cons(a, b))

  def length: Int =
    foldRight(0)((_, b) => 1 + b)

  def append[AA >: A](other: List[AA]): List[AA] =
    foldRight(other)(Cons(_, _))

  def ++[AA >: A](other: List[AA]): List[AA] =
    append(other)

  def prepend[AA >: A](a: AA): List[AA] = Cons(a, this)

  def ::[AA >: A](a: AA): List[AA] = prepend(a)

  def flatten[AA >: A, B](using ev: AA =:= List[B]): List[B] = this match
    case Nil           => Nil: List[B]
    case Cons(xs, xss) => xs ++ xss.flatten

  def foldRight[B](z: B)(f: (A, => B) => B): B =
    this.reverse.foldLeft(z)((b, a) => f(a, b))

  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(acc: B)(remaining: List[A]): B = {
      remaining match
        case Nil        => acc
        case Cons(h, t) => loop(f(acc, h))(t)
    }

    loop(z)(this)
  }

  def forall(p: A => Boolean): Boolean =
    foldRight(true)(p(_) && _)

  def map[B](f: A => B): List[B] = this match
    case Nil        => Nil
    case Cons(h, t) => Cons(f(h), t.map(f))

  def filter(p: A => Boolean): List[A] =
    flatMap {
      case x if p(x) => Cons(x, Nil)
      case _         => Nil
    }

  def flatMap[B](f: A => List[B]): List[B] =
    map(f).flatten

  def zipWith[B, C](other: List[B])(f: (A, B) => C): List[C] =
    (this, other) match
      case (Nil, Nil)                 => Nil
      case (la, Nil)                  => la.asInstanceOf[List[C]]
      case (Nil, lb)                  => lb.asInstanceOf[List[C]]
      case (Cons(h, t), Cons(oh, ot)) => Cons(f(h, oh), t.zipWith(ot)(f))

  def hasSubsequence[AA >: A](other: List[AA]): Boolean = {
    def loop(prev: Boolean)(xs: List[AA])(ys: List[AA]): Boolean = {
      (xs, ys) match
        case (Nil, Nil) => true
        case (_, Nil)   => true
        case (Nil, _)   => false
        case (Cons(xp, xsp), Cons(yp, ysp)) =>
          if xp == yp
          then loop(true)(xsp)(ysp)
          else if prev then false
          else loop(prev)(xsp)(ys)
    }

    loop(false)(this)(other)
  }
}

object List {
  def sums(in: List[Int]): Int = in match
    case Nil        => 0
    case Cons(h, t) => h + sums(t)

  def products(in: List[Int]): Int = in match
    case Nil        => 1
    case Cons(h, t) => h + products(t)

  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil else Cons(as.head, apply(as.tail: _*))

}
