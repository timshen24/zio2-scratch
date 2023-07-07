package official_documentation.get_started

import zio.*
import zio.Console.*

import java.io.IOException

object MyApp extends ZIOAppDefault {
  def run: ZIO[Any, IOException, Unit] = myAppLogic

  private val myAppLogic =
    val echo = Console.readLine.flatMap(line => Console.printLine(line))
    for {
      _ <- printLine("Hello! What is your name?")
      name <- readLine
      _ <- printLine(s"Hello, ${name}, welcome to ZIO!")
    } yield ()
}
