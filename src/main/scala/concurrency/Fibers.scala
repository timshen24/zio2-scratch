package concurrency

import zio.*
import utilsScala2.*

import java.io._

object Fibers extends ZIOAppDefault {
  val meaningOfLife: ZIO[Any, Nothing, Int] = ZIO.succeed(42)
  val favLang: ZIO[Any, Nothing, String] = ZIO.succeed("Scala")

  // Fiber = light weight thread
  //  def createFiber: Fiber[Throwable, String] = ??? // Fibers are impossible to create manually

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

  /**
   * Exercises
   */
  // 1 - zip two fibers without using the zip combinators
  // hints: create a fiber that waits for both
  // 因为没有掌握后面的知识，现在只能这么做
  def zipFibers[E, A, B](fiber1: Fiber[E, A], fiber2: Fiber[E, B]): ZIO[Any, Nothing, Fiber[E, (A, B)]] = {
    val finalEffect = for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield (v1, v2)
    finalEffect.fork
  }

  val zippedFibers_v2: ZIO[Any, Nothing, (String, String)] = for {
    fib1 <- ZIO.succeed("Result from fiber1").debugThread.fork
    fib2 <- ZIO.succeed("Result from fiber2").debugThread.fork
    fiber <- zipFibers(fib1, fib2)
    tuple <- fiber.join
  } yield tuple

  /**
   * What if fiber1 and fiber2 has different error type E1 and E2? We have to generalize it!
   * same implementation
   */
  def zipFibersGeneral[E, E1 <: E, E2 <: E, A, B](fiber1: Fiber[E1, A], fiber2: Fiber[E2, B]): ZIO[Any, Nothing, Fiber[E, (A, B)]] = {
    val finalEffect = for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield (v1, v2)
    finalEffect.fork
  }

  // 2 - same thing with orElse
  def chainFibers[E, A](fiber1: Fiber[E, A], fiber2: Fiber[E, A]): ZIO[Any, Nothing, Fiber[E, A]] =
      val waitFiber1 = fiber1.join
      val waitFiber2 = fiber2.join
      val finalEffect = waitFiber1.orElse(waitFiber2)
      finalEffect.fork


  // 3 - distributing a task in between many fibers
  // spawn n fibers, count the number of words each file, then aggregate all the result together in one big number
  // mimic mapreduce
  def generateRandomFile(path: String): Unit =
    val random = scala.util.Random
    val chars = 'a' to 'z'
    val nWords = random.nextInt(2000) // at most 2000
    val content = (1 to nWords)
      .map(_ => (1 to random.nextInt(10)).map(_ => chars(random.nextInt(26))).mkString) // one word for every 1 to nWords
      .mkString(" ")

    val writer = new FileWriter(new File(path))
    writer.write(content)
    writer.flush()
    writer.close()

  // part1 - an effect which reads one file and counts the words there
  def countWords(path: String): UIO[Int] =
    ZIO.succeed {
      val source = scala.io.Source.fromFile(path)
      val nWords = source.getLines().mkString(" ").split(" ").count(_.nonEmpty)
      println(s"Counted $nWords in $path")
      source.close()
      nWords
    }

 // part2 - spin up fibers for all the files
 def wordCountParallel(n: Int): UIO[Int] =
   val effects: Seq[ZIO[Any, Nothing, Int]] = (1 to n)
     .map(i => s"src/main/resources/testfile_$i.txt")
     .map(countWords) // list of effects returning fibers
     .map(_.fork)
     .map((fiberEff: ZIO[Any, Nothing, Fiber[Nothing, Int]]) => fiberEff.flatMap(_.join))
   effects.reduce { (zioa, ziob) =>
     for {
       a <- zioa
       b <- ziob
     } yield a + b
   }

  /**
   * How to test wordCountParallel?
    * cd src/main/resources/
   *  find . | -xargs wc
   */

  //  override def run: ZIO[Any, Any, Any] = differentThreadIO
  //  override def run: ZIO[Any, Any, Any] = runOnAnotherThread(meaningOfLife).debugThread
//  override def run: ZIO[Any, Any, Any] = runOnAnotherThread_v2(meaningOfLife).debugThread
//  override def run: ZIO[Any, Any, Any] = peekFiber.debugThread
//  override def run: ZIO[Any, Any, Any] = zippedFibers.debugThread
//  override def run: ZIO[Any, Any, Any] = chainedFibers.debugThread
//  override def run: ZIO[Any, Any, Any] = zippedFibers_v2.debugThread

//  override def run: ZIO[Any, Any, Any] = peekFiber.debugThread // None
  override def run: ZIO[Any, Any, Any] = ZIO.succeed((1 to 10).foreach(i => generateRandomFile(s"src/main/resources/testFile_$i.txt"))) *> wordCountParallel(10).debugThread
}
