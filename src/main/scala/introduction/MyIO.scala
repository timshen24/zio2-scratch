package introduction

import scala.io.StdIn._

object MyIO {
  case class MyIO[A](unsafeRun:() => A) {
    def map[B](f: A => B): MyIO[B] = {
      MyIO(() => f(unsafeRun()))
    }

    def flatMap[B](f: A => MyIO[B]): MyIO[B] = {
      MyIO(() => f(unsafeRun())).unsafeRun()
    }
  }

  case class MyZIO[-R, +E, +A](unsafeRun: R => Either[E, A]) {
    def map[B](f: A => B): MyZIO[R, E, B] = {
      MyZIO(r => unsafeRun(r) match {
        case Left(e) => Left(e)
        case Right(v) => Right(f(v))})
    }

    // E1 is the father of E, R1 is the son of R
    def flatMap[R1 <: R, E1 >: E, B](f: A => MyZIO[R1, E1, B]): MyZIO[R1, E1, B] = {
      MyZIO(r => unsafeRun(r) match {
        case Left(e) => Left(e)
        case Right(v) => f(v).unsafeRun(r)})
    }
  }

  def someDuration(): Unit = Thread.sleep(3000)

  def main(args: Array[String]): Unit = {
    /**
     * Create some IO which:
     * 1. measure the current time of the system
     * 2. measure the duration of a computation
     *  - use exercise 1
     *    3. read something from the console
     *    4. print something to the console (e.g. "What's your name?"), then read, then print a welcome message
     */
    val myIO1: MyIO[Long] = MyIO(() => System.currentTimeMillis())

    val myIO2: MyIO[Long] = for {
      startTime <- myIO1
      _ <- MyIO(() => someDuration())
      endTime <- myIO1
    } yield endTime - startTime
    println(myIO2.unsafeRun())

    val myIO2Equivalent = MyIO(
      () =>
        val startTime = System.currentTimeMillis()
        someDuration()
        val endTime = System.currentTimeMillis()
        (endTime - startTime, ())
    )
    println(myIO2Equivalent.unsafeRun())

    val myIO3With4: MyIO[Unit] = for {
      _ <- MyIO(() => println("Please enter your name:"))
      name <- MyIO[String](() => readLine())
      _ <- MyIO(() => println(s"Welcome to ZIO2, $name"))
    } yield ()
    myIO3With4.unsafeRun()
  }
}
