package concurrency

import zio._
import utilsScala2._

object Fibers extends ZIOAppDefault {
  val meaningOfLife = ZIO.succeed(42)
  val favLang = ZIO.succeed("Scala")

  // Fiber = light weight thread
  def createFiber: Fiber[Throwable, String] = ??? // Fibers are impossible to create manually

  val sameThreadIO: ZIO[Any, Nothing, (Int, String)] = for {
    mol <- meaningOfLife.debugThread
    lang <- favLang.debugThread
  } yield (mol, lang)

  val differentThreadIO: ZIO[Any, Nothing, Unit] = for {
    _ <- meaningOfLife.debugThread.fork
    _ <- favLang.debugThread.fork
  } yield ()

  val meaningOfLifeFiber: ZIO[Any, Nothing, Fiber[Throwable, Int]] = meaningOfLife.fork

  // join a fiber
  def runOnAnotherThread[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = for {
    fib <- zio.fork
    result <- fib.join // wait until this fiber to complete by joining it
  } yield result

  // awaiting a fiber, wait for it to finish but not necessarily for its result, but for its exit data structure
  def runOnAnotherThread_v2[R, E, A](zio: ZIO[R, E, A]): /*ZIO[R, Nothing, Exit[E, A]]*/ ZIO[R, Nothing, String] = for {
    fib <- zio.fork
    result <- fib.await
  } yield result match {
    case Exit.Success(value) => s"succeeded with $value"
    case Exit.Failure(cause) => s"failed with $cause"
  }

  // poll - peek at the result of the fiber RIGHT NOW, without blocking
  val peekFiber: ZIO[Any, Nothing, Option[Exit[Throwable, Int]]] = for {
    fib <- ZIO.attempt {
      Thread.sleep(1000)
      42
    }.fork
    result <- fib.poll
  } yield result

  // compose fibers
  val zippedFibers: ZIO[Any, Nothing, (String, String)] = for {
    fib1 <- ZIO.succeed("Result from fiber1").debugThread.fork
    fib2 <- ZIO.succeed("Result from fiber2").debugThread.fork
    /*fiber = fib1.zip(fib2)
    tuple <- fiber.join*/
    tuple <- fib1.zip(fib2).join
  } yield tuple

  // orElse
  val chainedFibers: ZIO[Any, Nothing, IO[Nothing, String]] = for {
    fiber1 <- ZIO.fail("not good").debugThread.fork
    fiber2 <- ZIO.succeed("Rock the jvm!").debugThread.fork
    fiber = fiber1 orElse fiber2
  } yield fiber.join

  //  override def run: ZIO[Any, Any, Any] = differentThreadIO
  //  override def run: ZIO[Any, Any, Any] = runOnAnotherThread(meaningOfLife).debugThread
//  override def run: ZIO[Any, Any, Any] = runOnAnotherThread_v2(meaningOfLife).debugThread
//  override def run: ZIO[Any, Any, Any] = peekFiber.debugThread
//  override def run: ZIO[Any, Any, Any] = zippedFibers.debugThread
  override def run: ZIO[Any, Any, Any] = chainedFibers.debugThread
}
