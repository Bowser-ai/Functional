package App

import Parallelism.Par
import Parallelism.Par.*

object ParApp {
  def sum(seq: IndexedSeq[Int]): Par[Int] = if seq.length < 2 then
    Par.unit(seq.headOption.getOrElse(0))
  else {
    val (l, r) = seq.splitAt(seq.length / 2)
    fork(sum(l)).map2(fork(sum(r)))(_ + _)
  }
  def main(args: Array[String]) = ???
}
