package introduction

import introduction.ZIOEffects.fibZIO
import zio.*

import scala.io.StdIn

object ZIOEffects {
  val meaningOfLife: ZIO[Any, Nothing, Int] = ZIO.succeed(42)
  val aFailure: ZIO[Any, String, Nothing] = ZIO.fail("Something went wrong")
  val aSuspendedZIO: ZIO[Any, Throwable, Int] = ZIO.suspend(meaningOfLife)

  // map + flatMap
  val improvedMOL: ZIO[Any, Nothing, RuntimeFlags] = meaningOfLife.map(_ * 2)
  val printingMOL: ZIO[Any, Nothing, Unit] = meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))

  val smallProgram: Any = for {
    _ <- ZIO.succeed(println("what's your name"))
    name <- ZIO.succeed(StdIn.readLine())
    _ <- ZIO.succeed(println(s"Welcome to ZIO, $name"))
  } yield ()

  val anotherMOL: ZIO[Any, Nothing, Int] = ZIO.succeed(100)
  val tupledZIO: ZIO[Any, Nothing, (Int, Int)] = meaningOfLife.zip(anotherMOL)
  val combinedZIO: ZIO[Any, Nothing, RuntimeFlags] = meaningOfLife.zipWith(anotherMOL)(_ * _)

  /**
   * Type aliases of ZIOs
   * UIO[A] = ZIO[Any, Nothing, A] - no requirements, can't fail, produces A
   * URIO[A] = ZIO[R, Nothing, A] - can't fail, produces A
   * RIO[R, A] = ZIO[R, Throwable, A] - can fail with a Throwable
   */
  val aUIO: UIO[Int] = ZIO.succeed(99)
  val aURIO: URIO[Int, Int] = ZIO.succeed(67)
  val anRIO: RIO[Int, Int] = ZIO.succeed(98)
  val aFailedRIO: RIO[Int, Int] = ZIO.fail(new RuntimeException("RIO failed"))
  /**
   * Task[A] = ZIO[Any, Throwable, A] - no requirements, can fail with a Throwable, produces A
   */
  val aSuccessfulTask: Task[Int] = ZIO.succeed(89)
  val aFailedTask: Task[Int] = ZIO.fail(new RuntimeException("Something bad"))

  /**
   * IO[E, A] = ZIO[Any, E, A] - no requirements
   */
  val aSuccessfulIO: IO[String, Int] = ZIO.succeed(34)
  val aFailedIO: IO[String, Int] = ZIO.fail("Something bad happened")

  /**
   * Exercise
   */

  // 1 - sequence two ZIOs and take the value of the last one
  def sequenceTakeLast[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] = {
    zioa.flatMap(_ => ziob.map(b => b))
//    zioa *> ziob
  }

  def sequenceTakeLastElegantly[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] = {
    zioa *> ziob
  }

  // 2 - sequence two ZIOs and take the value of the first one
  def sequenceTakeFirst[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] = {
    for {
      a <- zioa
      _ <- ziob
    } yield a
  }

  def sequenceTakeFirstElegantly[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] = {
    zioa <* ziob
  }

  //3 - run a ZIO forever
  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    zio.flatMap(_ => runForever(zio))
  }

  def runForever_v2[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    zio *> runForever_v2(zio)
  }

  val endlessLoop: ZIO[Any, Nothing, Unit] = runForever {
    ZIO.succeed {
      println("running...")
      Thread.sleep(1000)
    }
  }

  val endlessLoop_2: ZIO[Any, Nothing, Unit] = runForever_v2 {
    ZIO.succeed {
      println("running...")
      Thread.sleep(1000)
    }
  }

  // 4 - convert the value of a ZIO to sth else
  def convert[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] = {
    zio.map(_ => value)
  }

  def convertElegantly[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] = {
    zio.as(value)
  }

  // 5 - discard the value of a ZIO to unit
  def asUnit[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] =
    convert(zio, ())

  def asUnitElegantly[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] =
    zio.unit

  // 6 - recursion
  def sum(n: Int): Int = if n == 0 then 0 else n + sum(n - 1)

  def sumZIO(n: Int): UIO[Int] =
    if (n == 0) {
      ZIO.succeed(0)
    } else {
      sumZIO(n - 1).flatMap(prev => ZIO.succeed(prev + n))
    }

  // 7 - fibonacci
  // hint: use ZIO.suspend/ZIO.suspendSuccess
  def fibZIO(n: Int): UIO[BigInt] =
    if (n < 3) ZIO.succeed(1)
    else for {
      n1 <- ZIO.suspendSucceed(fibZIO(n - 1))
      n2 <- fibZIO(n - 2)
    } yield n1 + n2

  def main(args: Array[String]): Unit = {
    val runtime = Runtime.default
    Unsafe.unsafe { implicit unsafe =>
      //      val mol = Runtime.default.unsafe.run(meaningOfLife)
      //      println(mol)
      val firstEffect = ZIO.succeed {
        println("computing first effect...")
        Thread.sleep(100)
        1
      }

      val secondEffect = ZIO.succeed {
        println("computing second effect...")
        Thread.sleep(100)
        2
      }

      println(runtime.unsafe.run(sequenceTakeLast(firstEffect, secondEffect)))
      println(runtime.unsafe.run(sequenceTakeFirst(firstEffect, secondEffect)))
//      println(Runtime.default.unsafe.run(endlessLoop_2))
      println(runtime.unsafe.run(sumZIO(20)))
      println(runtime.unsafe.run(fibZIO(50000)))
    }

  }
}
