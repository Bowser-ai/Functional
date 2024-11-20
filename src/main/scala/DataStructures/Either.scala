package DataStructures

import scala.List

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](other: => Either[EE, B])(
      f: (A, B) => C
  ): Either[EE, C]
  def map2Both[EE >: E, B, C](other: => Either[EE, B])(
      f: (A, B) => C
  ): Either[Seq[EE], C]
}

object Either {
  case class Left[+E](e: E) extends Either[E, Nothing]:
    override def map[B](f: Nothing => B): Either[E, B] = this

    override def flatMap[EE >: E, B](
        f: Nothing => Either[EE, B]
    ): Either[EE, B] = this

    override def orElse[EE >: E, B >: Nothing](
        b: => Either[EE, B]
    ): Either[EE, B] = b

    override def map2[EE >: E, B, C](other: => Either[EE, B])(
        f: (Nothing, B) => C
    ): Either[E, C] = this

    override def map2Both[EE >: E, B, C](other: => Either[EE, B])(
        f: (Nothing, B) => C
    ): Either[Seq[EE], C] =
      other match
        case Left(e2)  => Left(List(e, e2))
        case Right(a2) => Left(List(e))

  case class Right[+A](a: A) extends Either[Nothing, A]:
    override def map[B](f: A => B): Either[Nothing, B] =
      Right(f(a))

    override def flatMap[EE >: Nothing, B](
        f: A => Either[EE, B]
    ): Either[EE, B] = f(a)

    override def orElse[EE >: Nothing, B >: A](
        b: => Either[EE, B]
    ): Either[EE, B] = this

    override def map2[EE >: Nothing, B, C](other: => Either[EE, B])(
        f: (A, B) => C
    ): Either[EE, C] = for {
      a <- this
      b <- other
    } yield f(a, b)

    override def map2Both[EE >: Nothing, B, C](other: => Either[EE, B])(
        f: (A, B) => C
    ): Either[Seq[EE], C] =
      other match
        case Left(e2)  => Left(List(e2))
        case Right(a2) => Right(f(a, a2))

  def sequence[E, A](source: List[Either[E, A]]): Either[E, List[A]] =
    traverse(source)(identity)

  def traverse[E, A, B](source: List[A])(
      f: A => Either[E, B]
  ): Either[E, List[B]] = source match
    case Nil    => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
}
