package coordination

import zio._
import utilsScala2._

/**
 * Promise is a primitive for waiting for ZIO while other zio completes with a value. You blocked until somebody else
 * from another fiber or another thread completes with a value
 */
object Promises extends ZIOAppDefault {

  val aPromise: ZIO[Any, Nothing, Promise[Throwable, Int]] = Promise.make[Throwable, Int]

  // await - block the fiber until the promise has a value
  val reader = aPromise.flatMap {
    promise => promise.await
  }

  // succeed, fail, complete
  val writer = aPromise.flatMap { promise =>
    promise.succeed(42)
//    promise.fail(new Exception("..."))
//    promise.complete(42)
  }

  def demoPromise(): Task[Unit] = {
    // producer - consumer problem
    def consumer(promise: Promise[Throwable, Int]): Task[Unit] = for {
      _ <- ZIO.succeed("[consumer] waiting for result...").debugThread
      mol <- promise.await
      _ <- ZIO.succeed(s"[consumer] I got the result: $mol").debugThread
    } yield ()

    def producer(promise: Promise[Throwable, Int]): Task[Unit] = for {
      _ <- ZIO.succeed("[producer] crunching numbers...").debugThread
      _ <- ZIO.sleep(3.seconds)
      _ <- ZIO.succeed("[producer] complete.").debugThread
      mol <- ZIO.succeed(42)
      _ <- promise.succeed(mol)
    } yield ()

    for {
      promise <- Promise.make[Throwable, Int]
//      _ <- consumer(promise) zip producer(promise) no use at all!!
      _ <- consumer(promise) zipPar producer(promise)
    } yield ()
  }

  /*  the above example:
      - purely functional block on a fiber until you get a signal from another fiber
      - waiting on a value which may not yet be available, without thread starvation
      - inter-fiber communication
     */

  // simulate downloading from multiple parts
  val fileParts = List("I ", "love S", "cala", " with pure FP an", "d ZIO! <EOF>")

  def downloadFileWithRef(): Task[Unit] = {
    def downloadFile(contentRef: Ref[String]): Task[Unit] =
      ZIO.collectAllDiscard( // Download all parts in sequence
        fileParts.map { part =>
          ZIO.succeed(s"got '$part'").debugThread *> ZIO.sleep(1.second) *> contentRef.update(_ + part)
        }
      )

    def notifyFileComplete(contentRef: Ref[String]): Task[Unit] = for { // using contentRef will resort to check the status again and again...util it is completed
      file <- contentRef.get
      _ <- if (file.endsWith("<EOF>")) ZIO.succeed("File download complete.").debugThread
      else ZIO.succeed("downloading...").debugThread *> ZIO.sleep(500.millis) *> notifyFileComplete(contentRef)
    } yield ()

    for {
      contentRef <- Ref.make("")
      _ <- downloadFile(contentRef) zipPar notifyFileComplete(contentRef)
    } yield ()
  }

  def downloadFileWithRefPromise(): Task[Unit] = {
    def downloadFile(contentRef: Ref[String], promise: Promise[Throwable, String]): Task[Unit] =
      ZIO.collectAllDiscard(
        fileParts.map { part =>
          for {
            _ <- ZIO.succeed(s"got '$part'").debugThread
            _ <- ZIO.sleep(1.second)
            file <- contentRef.updateAndGet(_ + part)
            _ <- if (file.endsWith("<EOF>")) promise.succeed(file) else ZIO.unit
          } yield ()
        }
      )

    def notifyFileComplete(contentRef: Ref[String], promise: Promise[Throwable, String]): Task[Unit] = for {
      _ <- ZIO.succeed("downloading...").debugThread
      file <- promise.await
      _ <- ZIO.succeed(s"file download complete: $file").debugThread
    } yield ()

    for {
      contentRef <- Ref.make("")
      promise <- Promise.make[Throwable, String]
      _ <- downloadFile(contentRef, promise) zipPar notifyFileComplete(contentRef, promise)
    } yield ()
  }

//  def run = demoPromise()
//  def run = downloadFileWithRef()
  def run = downloadFileWithRefPromise() // better than downloadFileWithRef()!!
}
