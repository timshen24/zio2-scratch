package introduction

import introduction.entities.Entities.*
import zio.*

import java.io.IOException
import java.util.concurrent.TimeUnit

object ZIODependencies extends ZIOAppDefault {
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
  val program: ZIO[Any, Throwable, Unit] = for {
    _ <- subscribe(User("Daniel", "daniel@rockthejvm.com"))
    _ <- subscribe(User("Bon Jovi", "jon@rockthejvm.com"))
  } yield ()

  // alternative!!
  def subscribe_v2(user: User): ZIO[UserSubscription, Throwable, Unit] = for {
    sub <- ZIO.service[UserSubscription] // ZIO[UserSubscription, Nothing, UserSubscription]
    _ <- sub.subscribeUser(user)
  } yield ()

  val program_v2: ZIO[UserSubscription, Throwable, Unit] = for {
    _ <- subscribe_v2(User("Daniel", "daniel@rockthejvm.com"))
    _ <- subscribe_v2(User("Bon Jovi", "jon@rockthejvm.com"))
  } yield ()

  /**
   * ZLayers
   */
  val connectionPoolLayer: ZLayer[Any, Nothing, ConnectionPool] = ZLayer.succeed(ConnectionPool.create(10))
  // a layer that requires a dependency (high layer) can be built with ZLayer.fromFunction
  // (and automatically fetch the function arguments and place them into the ZLayer's dependency/environment type argument)
  val databaseLayer: ZLayer[ConnectionPool, Nothing, UserDatabase] = ZLayer.fromFunction(UserDatabase.create _)
  val emailServiceLayer: ZLayer[Any, Nothing, EmailService] = ZLayer.succeed(EmailService.create())
  val userSubscriptionServiceLayer: ZLayer[UserDatabase with EmailService, Nothing, UserSubscription] = ZLayer.fromFunction(UserSubscription.create _)

  // composing layers
  // vertical composition >>>
  val databaseLayerFull: ZLayer[Any, Nothing, UserDatabase] = connectionPoolLayer >>> databaseLayer
  // horizontal composition: combine dependencies of both layers and the values of both layers
  val subscriptionRequirementsLayer: ZLayer[Any, Nothing, UserDatabase with EmailService] = databaseLayerFull ++ emailServiceLayer
  // mix & match
  val userSubscriptionLayer: ZLayer[Any, Nothing, UserSubscription] =
    subscriptionRequirementsLayer >>> userSubscriptionServiceLayer

  /**
   * - we don't need to care about dependencies till the end of the world
   * - all ZIOs requiring this dependency will use the same instance
   * - can use different instances of the same type for different needs (e.g for tests)
   * - layers can be created and composed much like regular ZIOs + rich API
   * @return
   */
  override def run: ZIO[Any, Throwable, Unit] = /*subscribe(User("Daniel", "daniel@rockthejvm.com"))*/ {
    /*program_v2.provideLayer(
      ZLayer.succeed(
        UserSubscription.create(
          EmailService.create(),
          UserDatabase.create(
            ConnectionPool.create(10)
          )
        )
      )
    )*/
    runnableProgram_v2
  }

  val runnableProgram: ZIO[Any, Throwable, Unit] = program_v2.provideLayer(userSubscriptionLayer)

  // magic
  val runnableProgram_v2: ZIO[Any, Throwable, Unit] = program_v2.provide(
    UserSubscription.live,
    EmailService.live,
    // ZIO will tell you if you're missing a layer
    UserDatabase.live,
    ConnectionPool.live(10),
    // ZIO will tell you a dependency graph!
//    ZLayer.Debug.tree,
    ZLayer.Debug.mermaid
  )

  // magic v2
  val userSubscriptionLayer_v2: ZLayer[Any, Nothing, UserSubscription] = ZLayer.make[UserSubscription](
    UserSubscription.live,
    EmailService.live,
    // ZIO will tell you if you're missing a layer
    UserDatabase.live,
    ConnectionPool.live(10),
  )

  // pass through, take the dependency and pass through the value channel
  val dbWithPoolLayer: ZLayer[ConnectionPool, Nothing, ConnectionPool with UserDatabase] = UserDatabase.live.passthrough // 把ZLayer[ConnectionPool, Nothing, UserDatabase] 变为ZLayer[ConnectionPool, Nothing, ConnectionPool with UserDatabase]

  // service = take a dep and expose it as a value to further layers
  val dbService: ZLayer[UserDatabase, Nothing, UserDatabase] = ZLayer.service[UserDatabase]

  // launch = creates a ZIO that users the services and never finishes. For entire application that is a layer
  val subscriptionLaunch: ZIO[EmailService with UserDatabase, Nothing, Nothing] = UserSubscription.live.launch

  // memorization
  /**
   * Already provided services in ZIO: Clock, Random, System, Console
   */
  val getTime: UIO[Long] = Clock.currentTime(TimeUnit.SECONDS)
  val randomValue: UIO[RuntimeFlags] = Random.nextInt
  val sysVariable: IO[SecurityException, Option[String]] = System.env("HADOOP_HOME")
  val printlnEffect: IO[IOException, Unit] = Console.printLine("This is ZIO")
}
