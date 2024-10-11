package DataStructures

import scala.annotation.tailrec

enum List[+A] {
  case Nil extends List[Nothing]
  case Cons(h: A, t: List[A]) extends List[A]

    def tail: List[A] = this match
      case Nil => throw new RuntimeException("Tail of empty list.")
      case Cons(h, t) => t

    def setHead[AA >: A](a: AA): List[AA] = this match
      case Nil => throw new RuntimeException("Cannot set head of empty list.")
      case Cons(_, t) => Cons(a, t)

    def init: List[A] = this match
      case Nil => throw new RuntimeException("Init of empty list.")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, t.init)
      
    def foldRight[B](z: B)(f: (A, B) => B): B = this match
      case Nil => z
      case Cons(h, t) => f(h, foldRight(z)(f))
      
    def length: Int =
      foldRight(0)((_, b) => 1 + b)
      
    def foldLeft[B](z: B)(f: (B, A) => B): B = {
      @tailrec
      def loop(acc: B)(remaining: List[A]): B = {
        remaining match
          case Nil => acc
          case Cons(h, t) => loop(f(acc, h))(t)
      }
    }
}

object List {
  def sums(in: List[Int]): Int = in match
    case Nil => 0
    case Cons(h, t) => h + sums(t)

  def products(in: List[Int]): Int = in match
    case Nil => 1
    case Cons(h, t) => h + products(t)

  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil else Cons(as.head, apply(as.tail: _*))
}