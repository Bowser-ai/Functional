import zio.Scope
import zio.test.*

import DataStructures.Option.*
import DataStructures.Option

object OptionSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Options")(
      test("GetOrElse") {
        val none = None
        assertTrue(none.getOrElse(55) == 55)
        val some = Some(12)
        assertTrue(some.getOrElse(55) == 12)
      },
      test("map")(
        check(Gen.int)(int =>
          assertTrue(
            None.map(_ => 55) == None,
            Some(int).map(_ + 1) ==
              Some(int + 1)
          )
        )
      ),
      test("flatMap")(
        check(Gen.int)(int =>
          assertTrue(
            None.flatMap(_ => Some(5)) == None,
            Some(int).flatMap(x => Some(x + 1)) == Some(int + 1)
          )
        )
      ),
      test("filter")(
        check(Gen.int)(int =>
          assertTrue(
            Some(int).filter(_ % 2 == 0) == (if int % 2 == 0 then Some(int)
                                             else None)
          )
        )
      ),
      test("sequence") {
        assertTrue(
          sequence(List(Some(1), Some(2), None, Some(3))) == None,
          sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3))
        )
      }
    )
}
