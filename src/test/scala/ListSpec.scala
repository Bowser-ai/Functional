import zio.Scope
import zio.test.*

import DataStructures.List
import DataStructures.List.*

object ListSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("hasSubSequence")(
      test("An empty list is a subsequence of another empty list") {
        val source = Nil
        val another = Nil
        assertTrue(source.hasSubsequence(another))
      },
      test("An empty list is a subsequence of another list") {
        val source = List(1, 2, 3)
        val another = Nil
        assertTrue(source.hasSubsequence(another))
      },
      test("Several subsequences of a list") {
        val source = List(1, 2, 3, 4)
        val target1 = List(List(1, 2), List(2, 3), List(4), Nil)
        assertTrue(target1.forall(source.hasSubsequence(_)))
      },
      test("Several lists that are NOT a subsequence of a list") {
        val source = List(1, 2, 3, 4)
        val target1 = List(List(1, 3), List(5), List(2, 4))
        assertTrue(target1.forall(!source.hasSubsequence(_)))
      }
    )
}
