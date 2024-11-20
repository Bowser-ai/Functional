package DataStructures

import scala.annotation.tailrec
import scala.{List, Option}

import Stream.*

sealed trait Stream[+A] {
  def headOption: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  def toList: List[A]

  def take(n: Int): Stream[A] =
    unfold(this -> n) {
      case (Empty, _)      => None
      case (Cons(h, t), k) => if k <= 0 then None else Some(h(), (t(), k - 1))
    }

  def drop(n: Int): Stream[A]

  def takeWhile(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Empty => None
      case Cons(h, t) =>
        val head = h()
        if p(head) then Some(head, t()) else None
    }

  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this -> other) {
      case (Empty, _)                 => None
      case (_, Empty)                 => None
      case (Cons(h, t), Cons(oh, ot)) => Some(f(h(), oh()), t() -> ot())
    }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this -> other) {
      case (Empty, Empty)        => None
      case (Cons(h, t), Empty)   => Some((Some(h()), None), (t(), empty))
      case (Empty, Cons(oh, ot)) => Some((None, Some(oh())), (empty, ot()))
      case (Cons(h, t), Cons(oh, ot)) =>
        Some((Some(h()), Some(oh())), (t(), ot()))
    }

  def dropWhile(p: A => Boolean): Stream[A]

  def forAll(p: A => Boolean): Boolean

  def foldRight[B](z: B)(f: (A, => B) => B): B

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = {
    def loop(acc: B)(scanned: Stream[B])(remaining: List[A]): Stream[B] = {
      remaining match
        case Nil    => cons(acc, scanned)
        case h :: t => loop(f(h, acc))(cons(acc, scanned))(t)
    }
    loop(z)(empty)(this.toList.reverse)
  }

  def scanRightViaFoldRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight(z -> Stream(z)) { case (head, (acc, scanned)) =>
      val newAcc = f(head, acc)
      newAcc -> cons(newAcc, scanned)
    }._2

  def map[B](f: A => B): Stream[B] =
    unfold(this) {
      case Empty      => None
      case Cons(h, t) => Some(f(h()), t())
    }

  def startsWith[AA >: A](other: Stream[AA]): Boolean = other match
    case Empty => false
    case otherCons =>
      zipAll(otherCons) forAll {
        case (None, None)         => false
        case (Some(_), None)      => true
        case (None, Some(_))      => false
        case (Some(x1), Some(x2)) => x1 == x2
      }

  def tails: Stream[Stream[A]] =
    unfold(this -> false) {
      case (Empty, finished) =>
        if finished then None else Some(empty, (empty, true))
      case (stream @ Cons(_, t), finished) => Some(stream, (t(), finished))
    }
}

object Stream {
  case object Empty extends Stream[Nothing] {
    override def toList: List[Nothing] = Nil

    override def drop(n: Int): Stream[Nothing] = empty

    override def dropWhile(p: Nothing => Boolean): Stream[Nothing] = empty

    override def forAll(p: Nothing => Boolean): Boolean = true

    override def foldRight[B](z: B)(f: (Nothing, => B) => B): B = z
  }

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
    override def toList: List[A] = {
      @tailrec
      def loop(acc: List[A])(remaining: Stream[A]): List[A] = {
        remaining match
          case Empty      => acc
          case Cons(h, t) => loop(h() :: acc)(t())
      }

      loop(Nil)(this).reverse
    }

    override def drop(n: Int): Stream[A] =
      if n <= 0 then this else t().drop(n - 1)

    override def dropWhile(p: A => Boolean): Stream[A] =
      if p(h()) then t().dropWhile(p) else this

    override def forAll(p: A => Boolean): Boolean =
      p(h()) && t().forAll(p)

    override def foldRight[B](z: B)(f: (A, => B) => B): B =
      f(h(), t().foldRight(z)(f))
  }

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    lazy val h = head
    lazy val t = tail
    Cons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](a: A*): Stream[A] =
    if a.isEmpty then empty else cons(a.head, apply(a.tail: _*))

  def const[A](a: A): Stream[A] =
    unfold(a)(s => Some(s, s))

  def from(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s + 1))

  def fibs: Stream[Int] =
    unfold((0, 1)) { case (s, a) => Some(s, (a, s + a)) }

  def unfold[S, A](s: S)(f: S => Option[(A, S)]): Stream[A] =
    f(s) match
      case Some((a, updatedState)) => cons(a, unfold(updatedState)(f))
      case None                    => empty
}
