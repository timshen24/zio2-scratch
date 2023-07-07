package official_documentation.get_started

import zio.*
import zio.Console.printLine
import zio.ZIO.*

import scala.concurrent.Future
import scala.util.Try

val s1: UIO[Int] = ZIO.succeed(42)
val f1: IO[String, Nothing] = ZIO.fail("Uh oh!")
val f2: IO[Exception, Nothing] = ZIO.fail(new Exception("oh no!"))

val zOption: IO[Option[Nothing], Int] = ZIO.fromOption(Some(2))
val zOption2: ZIO[Any, String, Int] = zOption.orElseFail("It wasn't there!")

case class User(id: String, teamId: String)

case class Team(teamId: String)

val maybeId: ZIO[Any, Option[Nothing], String] = ZIO.fromOption(Some("abc123"))

def getUser(userId: String): ZIO[Any, Throwable, Option[User]] = ZIO.succeed(Some(User("1", "2")))

def getTeam(teamId: String): ZIO[Any, Throwable, Team] =
  ZIO.fail(new Exception("fail to get Team"))

val result: Task[Option[(User, Team)]] = (for {
  id <- maybeId
  // Converts an option on values into an option on errors.
  // 原来是ZIO[Any, Throwable, Option[User]] 变成 ZIO[Any, Option[Throwable], User]
  // When we do this we move the possibility that the Option is None to the error channel, resulting in an Option[E]
  user <- getUser(id).some
  // Maps the error value of this effect to an optional value.
  // asSomeError厉害了，也是把ZIO[Any, Throwable, Team]变成ZIO[Any, Option[Throwable], Team]
  // 还有option方法也值得注意
  team <- getTeam(user.teamId).asSomeError
} yield (user, team)).unsome
  /**
   * 这个又厉害了，把ZIO[Any, Option[Throwable], (User, Team)]又变成了ZIO[Any, Throwable, Option[(User, Team)]
   */

val zEither: UIO[String] = ZIO.fromEither(Right("Success!"))
val zTry: Task[Double] = ZIO.fromTry(Try(42 / 0))

lazy val future = Future.successful("Hello!")
val zFuture: ZIO[Any, Throwable, String] =
  ZIO.fromFuture { implicit ec =>
    future.map(_ => "Goodbye!")
  }

/**
 * Convert any code into an effect
  */
import scala.io.StdIn
// 但ZIO已经有更好的Console.readLine方法了
val readLine: ZIO[Any, Throwable, String] =
  ZIO.attempt(StdIn.readLine())

def printLine(line: String): UIO[Unit] =
  ZIO.succeed(println(line))

import java.io.IOException

val readLine2: ZIO[Any, IOException, String] =
  // value的签名从Task[String]强转成了ZIO[Any, IOException, String]
  ZIO.attempt(StdIn.readLine()).refineToOrDie[IOException]

case class AuthError()
object legacy {
  def login(
             onSuccess: User => Unit,
             onFailure: AuthError => Unit): Unit = ???
}

val login: ZIO[Any, AuthError, User] =
  ZIO.async[Any, AuthError, User] { callback =>
    legacy.login(
      user => callback(ZIO.succeed(user)),
      err  => callback(ZIO.fail(err))
    )
  }

import scala.io.{ Codec, Source }

def download(url: String): Task[String] =
  ZIO.attempt {
    Source.fromURL(url)(Codec.UTF8).mkString
  }

def safeDownload(url: String): Task[String] =
  ZIO.blocking(download(url))

// 把阻塞线程转为ZIO effect
val sleeping =
  ZIO.attemptBlockingInterrupt(Thread.sleep(Long.MaxValue))

import java.net.ServerSocket
import zio.UIO

def accept(l: ServerSocket) =
  ZIO.attemptBlockingCancelable(l.accept())(ZIO.succeed(l.close()))

object Main extends ZIOAppDefault {
  def run: ZIO[Any, Throwable, Unit] = for {
    optionalRes <- result
    _ <- printLine(optionalRes.toString)
  } yield ()
}
