import zio.Scope
import zio.test.*

import DataStructures.Stream
import DataStructures.Stream.*

object StreamSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Streamtests.")(
      test("toList method")(check(Gen.listOf(Gen.int))(xs =>
        val aStream = Stream(xs: _*)
        val asList = aStream.toList
        assertTrue(asList == xs)
      )),
      test("take method")(check(Gen.listOf(Gen.int), Gen.int(0, 100)) {
        (xs, n) =>
          {
            val aStream = Stream(xs: _*)
            val aList = aStream.take(n).toList
            assertTrue(aList == xs.take(n))
          }
      }),
      test("drop method")(check(Gen.listOf(Gen.int), Gen.int(0, 100)) {
        (xs, n) =>
          {
            val aStream = Stream(xs: _*)
            val aList = aStream.drop(n).toList
            assertTrue(aList == xs.drop(n))
          }
      }),
      test("takewhile method")(check(Gen.listOf(Gen.int), Gen.int(0, 100)) {
        (xs, n) =>
          {
            val aStream = Stream(xs: _*)
            val aList = aStream.takeWhile(_ < n).toList
            assertTrue(aList == xs.takeWhile(_ < n))
          }
      }),
      test("dropwhile method")(check(Gen.listOf(Gen.int), Gen.int(0, 100)) {
        (xs, n) =>
          {
            val aStream = Stream(xs: _*)
            val aList = aStream.dropWhile(_ < n).toList
            assertTrue(aList == xs.dropWhile(_ < n))
          }
      }),
      test("forAll method")(check(Gen.listOf(Gen.int(0, 100))) { xs =>
        {
          val aStream = Stream(xs: _*)
          assertTrue(
            aStream.forAll(_ < 101),
            xs.isEmpty || !aStream.forAll(_ > 100)
          )
        }
      }),
      test("headOption") {
        val aStream = Stream(1)
        val anEmptyStream = Stream.empty
        assertTrue(
          aStream.headOption.contains(1),
          anEmptyStream.headOption.isEmpty
        )
      },
      test("from constructor") {
        val aStream = from(1).take(10)
        assertTrue(aStream.toList == (1 to 10))
      },
      test("fibs") {
        assertTrue(
          fibs.take(10).toList == List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
        )
      },
      test("map") {
        assertTrue(
          Stream.from(1).map(_ + 2).take(10).toList == List(3, 4, 5, 6, 7, 8, 9,
            10, 11, 12)
        )
      },
      test("zipAll")(check(Gen.listOf(Gen.int), Gen.listOf(Gen.int)) {
        (xs, ys) =>
          {
            val zipped = Stream(xs: _*).zipAll(Stream(ys: _*)).take(100).toList
            assertTrue(
              zipped == xs
                .map(Some(_))
                .zipAll(ys.map(Some(_)), None, None)
                .take(100)
            )
          }
      }),
      test("tails") {
        assertTrue(
          Stream(1, 2, 3).tails.map(_.toList).toList == List(
            List(1, 2, 3),
            List(2, 3),
            List(3),
            List()
          )
        )
      },
      test("ScanRight") {
        assertTrue(
          Stream(1, 2, 3).scanRight(0)(_ + _).toList == List(6, 5, 3, 0),
          Stream(1, 2, 3).scanRightViaFoldRight(0)(_ + _).toList == List(
            6,
            5,
            3,
            0
          )
        )
      }
    )
}
