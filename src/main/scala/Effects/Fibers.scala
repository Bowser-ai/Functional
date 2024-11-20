package Effects

import zio.*

def zipFiber[E, A, B](
    a: Fiber[E, A],
    b: Fiber[E, B]
): ZIO[Any, Nothing, Fiber[E, (A, B)]] = (for {
  x <- a.join
  y <- b.join
} yield x -> y).fork

def orElse[E, A, B](
    a: Fiber[E, A],
    b: Fiber[E, A]
): ZIO[Any, Nothing, Fiber[E, A]] = (for {
  exitA <- a.await
  res <- exitA match
    case Exit.Success(value) => ZIO.succeed(value)
    case Exit.Failure(cause) => b.join
} yield res).fork.onInterrupt(_ => a.interrupt *> b.interrupt)

object Fibers extends ZIOAppDefault {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = for {
    fibA <- ZIO.fail(RuntimeException("BOOM")).fork
    fibB <- ZIO.succeed(900).fork
    fibC <- orElse(fibA, fibB)
    res <- fibC.join
    _ <- Console.printLine(res)
  } yield ()
}
