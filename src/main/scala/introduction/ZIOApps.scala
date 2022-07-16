package introduction

import zio._

object ZIOApps {
  val meaningOfLife: UIO[Int] = ZIO.succeed(42)

  def main(args: Array[String]): Unit = {
    val runtime = Runtime.default
    given trace: Trace = Trace.empty
    Unsafe.unsafeCompat { unsafe =>
      given u: Unsafe = unsafe
      println(runtime.unsafe.run(meaningOfLife))
    }
  }
}

object BetterApp extends ZIOAppDefault {
  // provides runtime, trace, ...,
  override def run: ZIO[Any, Any, Any] =
//    ZIOApps.meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))
    ZIOApps.meaningOfLife.debug
}

// not needed
object ManualApp extends ZIOApp {
  override implicit def environmentTag = ???

  override type Environment = this.type

  override def bootstrap: ZLayer[Any, Any, ManualApp.type] = ???

  override def run: ZIO[Any, Any, Any] = ???
}