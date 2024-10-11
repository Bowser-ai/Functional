package Recursion

import scala.annotation.tailrec

object App {
  private def fac(n: Long): Long = {
    @tailrec
    def go(acc: Long, k: Long): Long = {
      if k <= 0 then acc else go(k * acc, k - 1)
    }
    go(1L, n)
  }

  private def fibo(n: Int): Int = {
    @tailrec
    def go(a: Int)(b: Int)(k: Int): Int = {
      if k <= 0 then a else go(b)(a + b)(k - 1)
    }
    go(0)(1)(n)
  }

  private def findFirst[A](as: Array[A])(p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if n >= as.length
      then -1
      else
        if p(as(n)) then n else loop(n + 1)
    }
    loop(0)
  }

  @tailrec
  private def isSorted[A](as: Array[A])(comp: (A, A) => Boolean): Boolean = {
    as.length match
      case x if x == 0 || x == 1 => true
      case x if x >= 2 => comp(as(0), as(1)) && isSorted(as.splitAt(2)._2)(comp)
  }

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C)(g: A => B): A => C =
    a => f(g(a))

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(0, 1, 2))(_ <= _))
    println(isSorted(Array(0, 2, 5, 5))(_ <= _))
    println(isSorted(Array.empty[Int])(_ <= _))
    println(isSorted(Array(344, 0, 2, 5, 5))(_ <= _))
  }
}
