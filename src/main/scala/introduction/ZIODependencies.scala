package introduction

import zio._

object ZIODependencies extends ZIOAppDefault {
  // app to subscribe users to newsletter
  case class User(name: String, email: String)

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
  }

  class EmailService {
    def email(user: User): Task[Unit] = ZIO.succeed(s"You" +
      s"ve just subscribed, ${user.name}").unit
  }

  object EmailService {
    def create(): EmailService = new EmailService
  }

  class UserDatabase(connectionPool: ConnectionPool) {
    def insert(user: User): Task[Unit] = for {
      conn <- connectionPool.get
      _ <- conn.runQuery(s"insert into subscribers(name, email) values (${user.name}, ${user.email})")
    } yield ()
  }

  object UserDatabase {
    def create(connectionPool: ConnectionPool) =
      new UserDatabase(connectionPool)
  }

  class ConnectionPool(nConnections: Int) {
    def get: Task[Connection] =
      ZIO.succeed(println("Acquired connection")) *> ZIO.succeed(Connection())
  }

  object ConnectionPool {
    def create(nConnection: Int) =
      new ConnectionPool(nConnection)
  }

  case class Connection() {
    def runQuery(query: String): Task[Unit] = ZIO.succeed(println(s"Executing $query"))
  }

  object Connection {

  }

  val subscriptionService: ZIO[Any, Nothing, UserSubscription] = ZIO.succeed { // Dependency injection
    UserSubscription.create(
      EmailService.create(),
      UserDatabase.create(
        ConnectionPool.create(10)
      )
    )
  }

  /**
   * Clean DI has drawbacks,
   * - does not scale for many services
   * - DI can be 100x worse
   *   - pass dependencies partially
   *   - not having all deps in the same place
   *   - passing dependencies multiple times
   *
   * @return
   */
  def subscribe(user: User): ZIO[Any, Throwable, Unit] = for {
    sub <- subscriptionService // service is instantiated at the point of call...it will be instantiated again and again, multiple time. We just need it instantiating only once.
    _ <- sub.subscribeUser(user)
  } yield ()

  // risk leaking resources if you subscribe multiple users in the same effect program
  val program = for {
    _ <- subscribe(User("Daniel", "daniel@rockthejvm.com"))
    _ <- subscribe(User("Bon Jovi", "jon@rockthejvm.com"))
  } yield ()

  // alternative!!
  def subscribe_v2(user: User): ZIO[UserSubscription, Throwable, Unit] = for {
    sub <- ZIO.service[UserSubscription] // ZIO[UserSubscription, Nothing, UserSubscription]
    _ <- sub.subscribeUser(user)
  } yield ()

  val program_v2 = for {
    _ <- subscribe_v2(User("Daniel", "daniel@rockthejvm.com"))
    _ <- subscribe_v2(User("Bon Jovi", "jon@rockthejvm.com"))
  } yield ()

  /**
   * - we don't need to care about dependencies till the end of the world
   * - all ZIOs requiring this dependency will use the same instance
   * - can use different instances of the same type for different needs (e.g for tests)
   * - layers can be created and composed much like regular ZIOs + rich API
   * @return
   */
  override def run: ZIO[Any, Throwable, Unit] = /*subscribe(User("Daniel", "daniel@rockthejvm.com"))*/ {
    program_v2.provideLayer(
      ZLayer.succeed(
        UserSubscription.create(
          EmailService.create(),
          UserDatabase.create(
            ConnectionPool.create(10)
          )
        )
      )
    )
  }
}
