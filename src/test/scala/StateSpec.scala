import State.Rand.*
import State.RNG.SimpleRNG
import State.State
import zio.Scope
import zio.test.*

object StateSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("RNG")(
      test("nonNegativeInt")(check(Gen.int, Gen.long) { (x, seed) =>
        val rng = SimpleRNG(seed)
        val (n, _) = nonNegativeInt.run(rng)
        assertTrue(n >= 0 && n <= Int.MaxValue)
      }),
      test("double")(check(Gen.int, Gen.long) { (x, seed) =>
        val rng = SimpleRNG(seed)
        val (n, _) = double.run(rng)
        zio.Console.printLine(n) *>
          assertTrue(n >= 0.0 && n < 1.0)
      }),
      test("ints")(check(Gen.int, Gen.long, Gen.int(0, 100)) {
        (x, seed, count) =>
          val rng = SimpleRNG(seed)
          val (lst, _) = ints(count).run(rng)
          zio.Console.printLine(lst) *>
            assertTrue(
              lst.forall(x => x <= Int.MaxValue && x >= Int.MinValue),
              lst.size == count
            )
      }),
      test("nonNegativeLessThan")(check(Gen.int(0, Int.MaxValue), Gen.long) {
        (x, seed) =>
          val rng = SimpleRNG(seed)
          val (n, _) = nonNegativeIntLessThan(x).run(rng)
          zio.Console.printLine(n) *>
            assertTrue(
              n >= 0 && n < x
            )
      }),
      test("stateAction")(check(Gen.int(0, Int.MaxValue))(x => {
        val state: State[Int, Int] = (2, _)
        assertTrue(State.get.run(x) == (2, x))

        val updatedState = for {
          v <- state
          _ <- State.modify[Int](_ * 2)
        } yield v
        val compareState: State[Int, Int] = s => (2, s * 2)
        assertTrue(updatedState.run(x)._2 == compareState.run(x)._2)
      }))
    )
}
