package concurrency

import zio._
import utilsScala2._

object Interruptions extends ZIOAppDefault {
  val zioWithTime = (ZIO.succeed("start computation").debugThread *>
    ZIO.sleep(2.seconds) *>
    ZIO.succeed(42).debugThread)
    .onInterrupt(ZIO.succeed("I was interrupted").debugThread)
  // onInterrupt onDone

  val interruption = for {
    fib <- zioWithTime.fork

    /** also an effect. This blocks the interruption （this）fiber until the calling fiber is either done or interrupted
     * * */
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _ <- ZIO.succeed("Interruption successful").debugThread
    result <- fib.join
  } yield result

  val interruption_v2 = for {
    fib <- zioWithTime.fork

    /** fiber is a simple datastructure stored on heap, thus very cheap. so a fiber never join although leaks, but not necessarily a problem * */
    //    _ <- (ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt).fork
    /** better than above line! * */
    _ <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interruptFork
    _ <- ZIO.succeed("Interruption successful").debugThread
    result <- fib.join
  } yield result


  /**
   * Automatic interruption
   */
  // outliving a parent fiber
  val parentEffect = ZIO.succeed("spawning fiber ").debugThread *>
    //zioWithTime.fork *> // child fibers will be automatically interrupted if parent fiber is completed for any reason
    zioWithTime.forkDaemon *> // to avoid this, you can use forkDaemon, then this fiber will be the fiber of the MAIN
    // fiber, not its parent's
    ZIO.sleep(1.second) *>
    ZIO.succeed("parent successful").debugThread

  val testOutlivingParent = for {
    parentEffectFib <- parentEffect.fork
    _ <- ZIO.sleep(3.seconds)
    _ <- parentEffectFib.join
  } yield () // child fibers will be automatically interrupted if parent fiber is completed for any reason

  // two fibers racing. The winner ends and the loser interrupted
  val slowEffect = (ZIO.sleep(2.seconds) *> ZIO.succeed("slow").debugThread).onInterrupt(ZIO.succeed("[slow] interrupted").debugThread)
  val fastEffect = (ZIO.sleep(1.seconds) *> ZIO.succeed("fast").debugThread).onInterrupt(ZIO.succeed("[fast] interrupted").debugThread)
  val aRace = slowEffect.race(fastEffect)
  val testRace = aRace.fork *> ZIO.sleep(3.seconds)

  /**
   * Exercises
   */
  // 1 - implement a timeout function
  def timeout[R, E, A](zio: ZIO[R, E, A], time: Duration): ZIO[R, E, A] =
    zio.onInterrupt(ZIO.succeed("been interrupted")).race(ZIO.sleep(time)).fork
    for {
      fib <- zio.fork
      _ <- (ZIO.sleep(time) *> fib.interrupt).fork // so as to not block the main fiber
      result <- fib.join
    } yield result

  val testTimeOut = timeout(
    ZIO.succeed("Starting ...").debugThread *> ZIO.sleep(2.seconds) *> ZIO.succeed("I made it!").debugThread, 1.second
  ).debugThread

  // 2 - timeout v2 =>
  def timeout_v2[R, E, A](zio: ZIO[R, E, A], time: Duration): ZIO[R, E, Option[A]] = {
    timeout(zio, time).foldCauseZIO(
      cause => if (cause.isInterrupted) ZIO.succeed(None) else ZIO.failCause(cause),
      value => ZIO.succeed(Some(value))
    )
  }

  val testTimeOut_v2 = timeout_v2(
    ZIO.succeed("Starting ...").debugThread *> ZIO.sleep(2.seconds) *> ZIO.succeed("I made it!").debugThread, 1.second
  ).debugThread

  //  def run = testRace
//  def run = testTimeOut
  def run = testTimeOut_v2
}
