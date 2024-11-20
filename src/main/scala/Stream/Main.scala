package Stream

import zio.*
import zio.Console.*
import zio.stream.*

val aStream: ZStream[Any, Nothing, Int] = ZStream.fromIterable(1 to 100)
val takeSink = ZSink.take[Int](5)

object Main extends ZIOAppDefault {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] =
    aStream.run(takeSink).flatMap(printLine(_))
}
