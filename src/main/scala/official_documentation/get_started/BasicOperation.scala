package official_documentation.get_started

import zio.*
import zio.Console.*
import zio.ZIO.*

import java.io.IOException

val succeeded: ZIO[Any, Nothing, Int] = ZIO.succeed(21).map(_ * 2)

/**
 * 注意，如果是ZIO.attmpt，mapError可以用.refineToOrDie[Exception]
 * 如果是ZIO.fail，就用mapError
 */
val failed: ZIO[Any, Exception, Unit] =
  ZIO.fail("No no!").mapError(msg => new Exception(msg))

val sequenced: ZIO[Any, IOException, Unit] =
  /**
   * In any chain of effects created with flatMap, the first failure will short-circuit the whole chain,
   * just like throwing an exception will prematurely exit a sequence of statements.
   */
  for {
    input <- readLine
    _ <- printLine(s"You entered: $input")
  } yield ()

/**
 * 先执行printLine，再执行readLine，得到(Unit, String)，*>取右半边结果，返回String。如果是<*，则取左半边结果，返回Unit
 */
val zipRight2: ZIO[Any, IOException, String] =
  Console.printLine("What is your name?") *>
    Console.readLine