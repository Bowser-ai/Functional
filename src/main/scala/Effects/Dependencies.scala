package Effects

import zio.*

case class User(name: String, email: String)

trait UserSubscription {
  def subscribe(user: User): Task[Unit]
}

object UserSubscription {
  val live: ZLayer[EmailService & UserService, Nothing, UserSubscription] =
    ZLayer(
      for {
        emailService <- ZIO.service[EmailService]
        userService <- ZIO.service[UserService]
      } yield new UserSubscription:
        override def subscribe(user: User): Task[Unit] = for {
          _ <- userService.insert(user)
          _ <- emailService.email(user)
        } yield ()
    )
}

trait EmailService {
  def email(user: User): Task[Unit]
}

object EmailService {
  val live: ZLayer[Any, Nothing, EmailService] = ZLayer.succeed(
    new EmailService:
      override def email(user: User): Task[Unit] = Console.printLine(
        s"You have been subscribed to Rock the JVM. Welcome ${user.name}."
      )
  )
}

trait UserService {
  def insert(user: User): Task[Unit]
}

object UserService {
  val live: ZLayer[ConnectionPool, Nothing, UserService] = ZLayer(
    for {
      pool <- ZIO.service[ConnectionPool]
    } yield new UserService:
      override def insert(user: User): Task[Unit] =
        pool.get.flatMap(con =>
          con.runQuery(
            "INSERT INTO USER(name, email) VALUES" +
              s"(${user.name}, ${user.email}"
          )
        )
  )
}

trait ConnectionPool {
  def get: Task[Connection]
}

object ConnectionPool {
  val live: ZLayer[Any, Nothing, ConnectionPool] =
    ZLayer.succeed(new ConnectionPool {
      override def get: Task[Connection] =
        Console.printLine("Acquired connection").as(Connection())
    })
}

case class Connection() {
  def runQuery(query: String): Task[Unit] =
    Console.printLine(s"Running query: $query.")
}

val appLayer = ConnectionPool.live >>> (UserService.live ++ EmailService.live)
  >>>
  UserSubscription.live

object Dependencies extends ZIOAppDefault {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = (for {
    _ <- ZIO.serviceWithZIO[UserSubscription](
      _.subscribe(User("Rosa", "RtheCat.hotmail.com"))
    )
  } yield ())
    .provideLayer(appLayer)
}
