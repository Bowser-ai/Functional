package App

import DataStructures.Option
import DataStructures.Option.*

object Calculations {
  def mean(source: Seq[Double]): Option[Double] =
    if source.isEmpty then None
    else Some(source.sum / source.size)

  def variance(source: Seq[Double]): Option[Double] =
    lift[Double, Double](Math.abs)(
      mean(source)
        .flatMap(m => mean(source.map(x => Math.pow(x - m, 2))))
    )

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch case _ => None

  def parseInts(ints: List[String]): Option[List[Int]] =
    traverse(ints)(i => Try(i.toInt))
}

object App {
  def main(args: Array[String]): Unit = ???
}
