package introduction

import zio._
import scala.util._

object ZIOErrorHandling extends ZIOAppDefault {
  // ZIOs can faile
  val aFailedZIO: IO[String, Nothing] = ZIO.fail("something went wrong")
  val failedWithThrowable: IO[RuntimeException, Nothing] = ZIO.fail(new RuntimeException("Boom!"))
  val failedWithDescription: ZIO[Any, String, Nothing] = failedWithThrowable.mapError(_.getMessage)

  // attempt: run an effect that might throw an Exception
  val badZIO: ZIO[Any, Nothing, RuntimeFlags] = ZIO.succeed {
    println("Trying something")
    val string: String = null
    string.length
  } // this is bad, as it will NPE

  // use attempt to replace succeed is a better practice
  val anAttempt: ZIO[Any, Throwable, Int] = ZIO.attempt {
    println("Trying something")
    val string: String = null
    string.length
  }

  // effectfully catch error
  anAttempt.catchAll {
    e => ZIO.succeed(s"returning a different value because $e")
  }
  val catchSelectivelyErrors: ZIO[Any, Throwable, Any] = anAttempt.catchSome {
    case e: RuntimeException => ZIO.succeed(s"Ignoring runtime Exception: $e")
    case _ => ZIO.succeed("Ignoring everything else")
  }

  val aBetterAttempt: ZIO[Any, Nothing, RuntimeFlags] = anAttempt.orElse(ZIO.succeed(56))

  val handleBoth: URIO[Any, String] = anAttempt.fold(ex => s"Something bad happened: $ex", value => s"Length of the string was $value")

  // effectful fold: foldZIO
  val handleBoth_v2: ZIO[Any, Nothing, String] = anAttempt.foldZIO(
    ex => ZIO.succeed(s"Something bad happened: $ex"),
    value => ZIO.succeed(s"Length of the string was $value")
  )

  /**
   * Conversions between Option/Try/Either to ZIO
   *
   * @return
   */
  val aTryToZIO: ZIO[Any, Throwable, Int] = ZIO.fromTry(Try(42 / 0))

  // Either --> ZIO
  val anEither: Either[Int, String] = Right("Success!")

  val anEitherToZIO: ZIO[Any, Int, String] = ZIO.fromEither(anEither)

  // ZIO -> ZIO with Either as the value channel
  val eitherZIO: URIO[Any, Either[Throwable, Int]] = anAttempt.either
  // reverse
  val anAttempt_v2: ZIO[Any, Throwable, Int] = eitherZIO.absolve

  // option -> ZIO
  val anOption: ZIO[Any, Option[Nothing], Int] = ZIO.fromOption(Some(42))

  /**
   * Exercise: implement a version of fromTry, fromOption, fromEither, either, absolve
   * using fold and foldZIO
   *
   * @return
   */
  def try2ZIO[A](aTry: Try[A]): Task[A] = {
    aTry match
      case Failure(exception) => ZIO.fail(exception)
      case Success(value) => ZIO.succeed(value)
  }

  def either2ZIO[A, B](anEither: Either[A, B]): ZIO[Any, A, B] = {
    anEither match
      case Left(value) => ZIO.fail(value)
      case Right(value) => ZIO.succeed(value)
  }

  def option2ZIO[A](anOption: Option[A]): ZIO[Any, Option[Nothing], A] = {
    anOption match
      case Some(value) => ZIO.succeed(value)
      case None => ZIO.fail(None)
  }

  def zio2ZioEither[R, A, B](zio: ZIO[R, A, B]): ZIO[R, Nothing, Either[A, B]] = {
    zio.foldZIO(
      error => ZIO.succeed(Left(error)),
      value => ZIO.succeed(Right(value))
    )
  }

  def absolveZio[R, A, B](zio: ZIO[R, Nothing, Either[A, B]]): ZIO[R, A, B] = {
    zio.flatMap {
      case Left(e) => ZIO.fail(e)
      case Right(value) => ZIO.succeed(value)
    }
  }

  override def run: ZIO[Any, Any, Any] = ???

}
