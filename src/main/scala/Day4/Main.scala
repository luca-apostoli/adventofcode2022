package Day4

import zio.Console.printLine
import Utils.Utils.lines
import zio.stream.ZSink
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Main extends ZIOAppDefault {

  override def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] = {
    for {
      _ <- printLine("Welcome to your Day4!")
      _ <- lines("day4.txt")
        .map {
          _.split(",").map(
            x =>
              RangeParser.parse(RangeParser.range, x) match {
                case RangeParser.Success(d, _) => d
            }
          )
        }
        .map(x => (x.head, x.apply(1)))
        .map {
          case (a, b) => {
            a intersect b
          }
        }
        .run(ZSink.foldLeft(0)((incr, abc) => {
          if (abc.isEmpty) {
            incr
          } else {
            incr + 1
          }
        }))
        .debug("Result")
    } yield ()
  }
}
