package introduction.entities

import zio._

object Entities {
  case class Connection() {
    def runQuery(query: String): Task[Unit] = ZIO.succeed(println(s"Executing $query"))
  }

  object Connection {

  }

  class ConnectionPool(nConnections: Int) {
    def get: Task[Connection] =
      ZIO.succeed(println("Acquired connection")) *> ZIO.succeed(Connection())
  }

  object ConnectionPool {
    def create(nConnection: Int) =
      new ConnectionPool(nConnection)

    def live(nConnections: Int): ZLayer[Any, Nothing, ConnectionPool] =
      ZLayer.succeed(create(nConnections))
  }

  class EmailService {
    def email(user: User): Task[Unit] = ZIO.succeed(s"You've just subscribed, ${user.name}").unit
  }

  object EmailService {
    def create(): EmailService = new EmailService

    val live: ZLayer[Any, Nothing, EmailService] = ZLayer.succeed(create())
  }

  // app to subscribe users to newsletter
  case class User(name: String, email: String)

  class UserDatabase(connectionPool: ConnectionPool) {
    def insert(user: User): Task[Unit] = for {
      conn <- connectionPool.get
      _ <- conn.runQuery(s"insert into subscribers(name, email) values (${user.name}, ${user.email})")
    } yield ()
  }

  object UserDatabase {
    def create(connectionPool: ConnectionPool) =
      new UserDatabase(connectionPool)

    val live: ZLayer[ConnectionPool, Nothing, UserDatabase] = ZLayer.fromFunction(create _)
  }

  class UserSubscription(emailService: EmailService, userDatabase: UserDatabase) {
    def subscribeUser(user: User): Task[Unit] = {
      for {
        _ <- emailService.email(user)
        _ <- userDatabase.insert(user)
      } yield ()
    }
  }

  object UserSubscription {
    def create(emailService: EmailService, userDatabase: UserDatabase) =
      new UserSubscription(emailService, userDatabase)

    /**
     * 课程后期后再在这里加
     */
    val live: ZLayer[EmailService with UserDatabase, Nothing, UserSubscription] = ZLayer.fromFunction(create _)
  }
}
