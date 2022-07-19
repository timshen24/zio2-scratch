package introduction

import zio.*

import java.io.IOException
import java.net.NoRouteToHostException
import scala.util.*

object ZIOErrorHandling extends ZIOAppDefault {
  // ZIOs can fail
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

  val aBetterAttempt: ZIO[Any, Nothing, Int] = anAttempt.orElse(ZIO.succeed(56))

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

  /**
   * Errors = failures present in the ZIO type signature ("checked" errors)
   * Defects = failures that are unrecoverable, unforeseen, NOT present in the ZIO type signature
   *
   * ZIO[R,E,A] can finish with Exit[E,A]
   * - Success[A] containing a value
   * - Cause[E]
   * - Fail[E] containing the error
   * - Die(t: Throwable)  which was unforeseen
   * */
  val divisionByZero: UIO[Int] = ZIO.succeed(1 / 0)

  val failedInt: ZIO[Any, String, Int] = ZIO.fail("I failed!")
  val failureCauseExposed: ZIO[Any, Cause[String], Int] = failedInt.sandbox // not only expose Fail but also unforeseen Throwable
  val failureCauseHidden: ZIO[Any, String, Int] = failureCauseExposed.unsandbox
  // fold with cause
  val foldedWithCause: URIO[Any, String] = failedInt.foldCause(
    cause => s"this failed with ${cause.defects}",
    value => s"this succeeded with $value"
  )
  val foldedWithCause_v2: ZIO[Any, Nothing, String] = failedInt.foldCauseZIO(
    cause => ZIO.succeed(s"this failed with ${cause.defects}"),
    value => ZIO.succeed(s"this succeeded with $value")
  )

  /*
  Good practice:
  - at a lower level, your "errors" should be treated
  - at a higher level, you should hide "errors" and assume they are unrecoverable
 */
  def callHTTPEndpoint(url: String): ZIO[Any, IOException, String] =
    ZIO.fail(new IOException("no internet, dummy!"))

  // Suppose you are a client, you should call it as a high level error handling
  val endpointCallWithDefects: ZIO[Any, Nothing, String] =
    callHTTPEndpoint("rockthejvm.com").orDie // all errors are now defects

  // refining the error channel
  def callHTTPEndpointWideError(url: String): ZIO[Any, Exception, String] =
    ZIO.fail(new IOException("No internet!!"))

  def callHTTPEndpoint_v2(url: String): ZIO[Any, IOException, String] =
    callHTTPEndpointWideError(url).refineOrDie[IOException] {
      case e: IOException => e
      case _: NoRouteToHostException => new IOException(s"No route to host to $url, can't fetch page")
    } // Exceptions not included in the partial function will fall into the 'OrDie' logic...

  // reverse: turn defects into the error channel
  val endpointCallWithError: ZIO[Any, String, String] = endpointCallWithDefects.unrefine {
    case e => e.getMessage
  }

  /**
   * Combine effects with different errors
   */
  case class IndexError0(message: String)

  case class DbError0(message: String)

  val callApi: ZIO[Any, IndexError0, String] = ZIO.succeed("page: <html></html>")
  val queryDb: ZIO[Any, DbError0, Int] = ZIO.succeed(1)
  val combined: ZIO[Any, IndexError0 | DbError0, (String, Int)] = for {
    page <- callApi
    rowsAffected <- queryDb
  } yield (page, rowsAffected) // PROBLEM: lost type safety

  /**
   * Solutions:
   * - design an error model
   * - or use Scala 3 union types: IndexError0 | DbError0
   * - .mapError to some common error type
   */
  trait AppError

  case class IndexError(message: String) extends AppError

  case class DbError(message: String) extends AppError

  /**
   * Exercises
   */
  // 1 - make this effect fail with a TYPED error
  val aBadFailure: ZIO[Any, Nothing, Int] = ZIO.succeed[Int](throw new RuntimeException("this is bad!"))
  val aBetterFailure: ZIO[Any, Cause[Nothing], Int] = aBadFailure.sandbox // exposes the defect in the Cause
  val aBetterFailure_v2: ZIO[Any, RuntimeException, Int] = aBadFailure.unrefine { // surfaces out the exception in the error channel
    case e: RuntimeException => e
  }

  // 2 - transform a zio into another zio with a narrower exception type
  def ioException[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] = {
    zio.refineOrDie {
      case ioe: IOException => ioe
    }
  }

  // 3
  def left[R, E, A, B](zio: ZIO[R, E, Either[A, B]]): ZIO[R, Either[E, A], B] =
    zio.foldZIO(
      e => ZIO.fail(Left(e)),
      {
        case Left(a) => ZIO.fail(Right(a))
        case Right(b) => ZIO.succeed(b)
      }
    )

  // 4
  val database: Map[String, Int] = Map(
    "daniel" -> 123,
    "alice" -> 789
  )
  case class QueryError(reason: String)
  case class UserProfile(name: String, phone: Int)

  def lookupProfile(userId: String): ZIO[Any, QueryError, Option[UserProfile]] =
    if (userId != userId.toLowerCase())
      ZIO.fail(QueryError("user ID format is invalid"))
    else
      ZIO.succeed(database.get(userId).map(phone => UserProfile(userId, phone)))

  // surface out all the failed cases of this API
  def betterLookupProfile(userId: String): ZIO[Any, Option[QueryError], UserProfile] =
    lookupProfile(userId).foldZIO(
      error => ZIO.fail(Some(error)),
      {
        case Some(profile) => ZIO.succeed(profile)
        case None => ZIO.fail(None)
      }
    )

  def betterLookupProfile_v2(userId: String): ZIO[Any, Option[QueryError], UserProfile] =
    lookupProfile(userId).some

  override def run: ZIO[Any, Any, Any] = {
    failureCauseExposed.debug
    //    failureCauseHidden.debug
    //    foldedWithCause.debug
  }

}
