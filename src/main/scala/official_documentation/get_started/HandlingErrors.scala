package official_documentation.get_started

import zio.*
import ZIO.*

import java.io.{FileNotFoundException, IOException}
import java.nio.file.*

val zeither: ZIO[Any, Nothing, Either[String, Nothing]] =
  ZIO.fail("Uh oh!").either

def openFile(filename: String): ZIO[Any, IOException, Array[Byte]] =
  ZIO.attempt(Files.readAllBytes(Paths.get(filename))).refineToOrDie[IOException]

val z: ZIO[Any, IOException, Array[Byte]] =
  openFile("primary.json").catchAll { error =>
    for {
      _    <- ZIO.logErrorCause("Could not open primary file", Cause.fail(error))
      file <- openFile("backup.json")
    } yield file
  }

val data: ZIO[Any, IOException, Array[Byte]] =
  openFile("primary.data").catchSome {
    case _ : FileNotFoundException =>
      openFile("backup.data")
  }

/**
 * 可以看出，catch和orElse都可以
 */
val primaryOrBackupData: ZIO[Any, IOException, Array[Byte]] =
  openFile("primary.data").orElse(openFile("backup.data"))

lazy val DefaultData: Array[Byte] = Array(0, 0)

val primaryOrDefaultData: ZIO[Any, Nothing, Array[Byte]] =
  openFile("primary.data").fold(
    _    => DefaultData, // Failure case
    data => data)        // Success case

/**
 * 五花八门的Error Handling，非常灵活
 * The foldZIO method is almost the most powerful error recovery method in ZIO, with only foldCauseZIO being more powerful.
 * Most other operators, such as either or orElse, are implemented in terms of these powerful methods.
 */
val primaryOrSecondaryData: ZIO[Any, IOException, Array[Byte]] =
  openFile("primary.data").foldZIO(
    _    => openFile("secondary.data"), // Error handler
    data => ZIO.succeed(data))          // Success handler

sealed trait Content
object Content {
  case class NoContent(error: IOException) extends Content
  // Add other cases as needed
}

case class DefaultData2()

def readUrls(str: String): ZIO[Any, IOException, String] = ???

def fetchContent(success: String): Task[String] = ???

val urls =
  readUrls("urls.json").foldZIO(
    ex => ZIO.succeed(Content.NoContent(ex)),
    success => fetchContent(success)
  )

val retryOpenFile: ZIO[Any, IOException, Array[Byte]] =
  openFile("primary.data")
//    .retry(Schedule.recurs(5))
    .retryOrElse(Schedule.recurs(5), (_, _) => ZIO.succeed(DefaultData))