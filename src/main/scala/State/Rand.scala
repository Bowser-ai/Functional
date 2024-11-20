package State

type Rand[A] = State[RNG, A]

object Rand {
  import State.*

  def int: Rand[Int] = _.nextInt

  def nonNegativeInt: Rand[Int] = rng => {
    val (n, newRNG) = rng.nextInt
    Math.abs(n) % Int.MaxValue -> newRNG
  }

  def nonNegativeIntLessThan(n: Int): Rand[Int] =
    nonNegativeInt.flatMap(x => {
      val mod = x % n
      if x + (n - 1) - mod >= 0 then unit(mod)
      else nonNegativeIntLessThan(n)
    })

  def double: Rand[Double] = nonNegativeInt
    .map(_ / (Int.MaxValue.toDouble + 1))

  def intDouble: Rand[(Int, Double)] = int.both(double)

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))
}
