package Day3

import zio.Console.printLine
import Utils.Utils.lines
import zio.stream.ZSink
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Main extends ZIOAppDefault {

  override def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] = {
    for {
      _ <- printLine("Welcome to your Day3!")
      _ <- lines("day3.txt")
        .zipWithPreviousAndNext
        .map{case (Some(a), b, Some(c))
            => Some(a intersect b intersect c)
          case _ => None
        }
        .run(
          ZSink.foldLeft((0,0))(
            (incr, abc) => {
              if (incr._2 % 3 == 0) {
                abc match {
                  case Some(a) => {
                    val v = a.toCharArray.head
                    if (v >= 'a' && v <= 'z') {
                      (v + incr._1 - 'a' + 1, incr._2 + 1)
                    } else {
                      (v + incr._1 - 'A' + 27, incr._2 + 1)
                    }
                  }
                  case _ => incr
                }
              } else {
                (incr._1, incr._2 + 1)
              }
            })
        )
        .debug("Result")
    } yield ()
  }
}
