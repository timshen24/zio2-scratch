package concurrency

import zio._
import utilsScala2._

object Parallelism extends ZIOAppDefault {

  val meaningOfLife = ZIO.succeed(42)
  val favLang = ZIO.succeed("Scala")
  val combined = meaningOfLife zip favLang // Combines/zips in a sequential way

  val combinedPar = meaningOfLife zipPar favLang // Combine in parallel

  /**
   * - start each on fibers
   * - what if one failed? The other one should be interrupted
   * - what if one interrupted? The entire thing should be interrupted
   * - what if the whole thing is interrupted? Need to interrupt both effects
   */
  // try a zipPar yourself
  // hint: fork/join/await/interrupt
  /**
   * This implementation is not 100% correct, as you not know which fiber finishes first
   * */
  def myZipPar[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, (A, B)] =
    val exits = for {
      fibA <- zioa.fork
      fibB <- ziob.fork
      exitA <- fibA.await
      exitB <- exitA match {
        case Exit.Success(value) => fibB.await
        case Exit.Failure(_) => fibB.interrupt
      }
    } yield (exitA, exitB)
    exits.flatMap {
      case (Exit.Success(a), Exit.Success(b)) => ZIO.succeed((a, b)) // happy path
      case (Exit.Success(_), Exit.Failure(cause)) => ZIO.failCause(cause)
      case (Exit.Failure(cause), Exit.Success(_)) => ZIO.failCause(cause)
      case (Exit.Failure(cause1), Exit.Failure(cause2)) => ZIO.failCause(cause1 && cause2)
    }

  // parallel combinators
  // zipPar, zipWithPar

  // collectAllPar
  val effects: Seq[ZIO[Any, Nothing, Int]] = (1 to 10).map(i => ZIO.succeed(i).debugThread)
  val collectedValues: ZIO[Any, Nothing, Seq[Int]] = ZIO.collectAllPar(effects) // traverse

  // foreachPar
  val printlnParallel = ZIO.foreachPar((1 to 10).toList)(i => ZIO.succeed(println(i)))

  // reduceAllPar, mergeAllPar
  val sumPar = ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _)
  val sumPar_v2 = ZIO.mergeAllPar(effects)(0)(_ + _)

  /**
   * if all effects are good, all good
   * one effects fails => every one else is interrupted, error is surfaced
   * one effect interrupted => every one else is interrupted, error = interrupution (for the big expression)
   * if the entire thing is interrupted => all effects are interrupted
   */

  /**
   * Exercise, do the word counting
   */
  def countWords(path: String): UIO[Int] =
    ZIO.succeed {
      val source = scala.io.Source.fromFile(path)
      val nWords = source.getLines().mkString(" ").split(" ").count(_.nonEmpty)
      println(s"Counted $nWords in $path")
      source.close()
      nWords
    }

  def wordCountParallel(n: Int): UIO[Int] =
    val effects = (1 to n).map(i=>countWords(s"src/main/resources/testfile_$i.txt"))
    //v1 collectAllPar
    ZIO.collectAllPar(effects).map(_.sum)
    //v2 mergeAllPar
    ZIO.mergeAllPar(effects)(0)(_+_)
    ZIO.reduceAllPar(ZIO.succeed(0), effects)(_+_)

  val allWords = ZIO.reduceAllPar(ZIO.succeed(0), (1 to 10).map(i=>countWords(s"src/main/resources/testfile_$i.txt").fork))

//  def run = combinedPar.debugThread
  //  def run = collectedValues.debugThread
//  def run = collectedValues.debugThread
  def run = wordCountParallel(10).debugThread
}
