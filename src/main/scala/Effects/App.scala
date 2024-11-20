package Effects

import zio.*
import zio.Console.*

import java.io.IOException

def sequenceTakeLast[R, E, A, B](
    zioA: ZIO[R, E, A]
)(zioB: ZIO[R, E, B]): ZIO[R, E, B] = zioA *> zioB

def sequenceTakeFirst[R, E, A, B](
    zioA: ZIO[R, E, A]
)(zioB: ZIO[R, E, B]): ZIO[R, E, A] = zioA <* zioB

def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
  zio *> runForever(zio)

def convert[R, E, A, B](zio: ZIO[R, E, A])(b: B): ZIO[R, E, B] = for {
  _ <- zio
} yield b

def asUnit[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] = for {
  _ <- zio
} yield ()

def sumZIO(n: Int): UIO[Int] =
  if n == 0 then ZIO.succeed(0)
  else ZIO.succeed(sumZIO(n - 1).map(n + _)).flatten

def fiboZIO(n: Int): UIO[BigInt] = {
  def loop(k: Int)(s: (Int, Int)): UIO[BigInt] = {
    if k >= n then ZIO.succeed(s._1)
    else loop(k + 1)(s._2, s._1 + s._2)
  }
  loop(0)(0, 1)
}

def failure: Task[Int] = for {
  value <- ZIO.random.flatMap(_.nextIntBetween(0, 100))
  _ <- ZIO.fail(RuntimeException("Boom")).when(value < 50)
} yield value

def fromEither[E, A](either: Either[E, A]): IO[E, A] =
  either match
    case Left(error)  => ZIO.fail(error)
    case Right(value) => ZIO.succeed(value)

def either[R, E, A](zio: ZIO[R, E, A]): URIO[R, Either[E, A]] =
  zio.fold(Left(_), Right(_))

enum Color {
  case red
  case green
  case orange
}

val aBadFailure = ZIO.succeed[Int](throw RuntimeException("Boom"))
val recoveredFailure = aBadFailure.unrefine { case e: RuntimeException =>
  e
}

def ioException[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] =
  zio.refineOrDie { case e: IOException =>
    e
  }

def left[R, E, A, B](zio: ZIO[R, E, Either[A, B]]): ZIO[R, Either[E, A], B] =
  zio.foldZIO(
    e => ZIO.fail(Left(e)),
    {
      case Left(a)  => ZIO.fail(Right(a))
      case Right(b) => ZIO.succeed(b)
    }
  )

val database = Map(
  "daniel" -> 123,
  "alice" -> 456
)

case class QueryError(reason: String)
case class UserProfile(name: String, phone: Int)

def lookupProfile(userId: String): ZIO[Any, Option[QueryError], UserProfile] =
  if userId != userId.toLowerCase then
    ZIO.fail(
      Some(
        QueryError(
          "Invalid " +
            "query"
        )
      )
    )
  else ZIO.fromOption(database.get(userId)).map(UserProfile(userId, _))

object App extends ZIOAppDefault {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    val effect = lookupProfile("daniel22")
    effect.flatMap(printLine(_))
}
