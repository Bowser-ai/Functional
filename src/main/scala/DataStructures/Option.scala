package DataStructures

import scala.List

sealed trait Option[+A] {
  def flatMap[B](f: A => Option[B]): Option[B]
  def map[B](f: A => B): Option[B]
  def getOrElse[AA >: A](default: => AA): AA
  def orElse[AA >: A](other: => Option[AA]): Option[AA]
  def filter(p: A => Boolean): Option[A]
  def map2[B, C](other: => Option[B])(f: (A, B) => C): Option[C]
}

object Option {
  case object None extends Option[Nothing]:
    override def flatMap[B](f: Nothing => Option[B]): Option[B] =
      this

    override def map[B](f: Nothing => B): Option[B] =
      this

    override def getOrElse[AA >: Nothing](default: => AA): AA =
      default

    override def orElse[AA >: Nothing](other: => Option[AA]): Option[AA] =
      other

    override def filter(p: Nothing => Boolean): Option[Nothing] =
      this

    override def map2[B, C](other: => Option[B])(
        f: (Nothing, B) => C
    ): Option[C] = this

  case class Some[+A](a: A) extends Option[A]:
    override def flatMap[B](f: A => Option[B]): Option[B] =
      f(a)

    override def map[B](f: A => B): Option[B] =
      Some(f(a))

    override def getOrElse[AA >: A](default: => AA): AA =
      a

    override def orElse[AA >: A](other: => Option[AA]): Option[AA] =
      this

    override def filter(p: A => Boolean): Option[A] =
      if p(a) then this else None

    override def map2[B, C](other: => Option[B])(f: (A, B) => C): Option[C] =
      for {
        a <- this
        b <- other
      } yield f(a, b)

  def sequence[A](source: List[Option[A]]): Option[List[A]] =
    traverse(source)(identity)

  def traverse[A, B](source: List[A])(f: A => Option[B]): Option[List[B]] =
    source match
      case Nil    => Some(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
}
