package Day2

import Day2.Results.{forceResult, stringToResultValue}
import Day2.PRS.{result, stringToPRSValue}
import zio.Console.printLine
import Utils.Utils.lines
import zio.stream.ZSink
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Main extends ZIOAppDefault {

  override def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] = {
    for {
      _ <- printLine("Welcome to your Day2!")
      _ <- lines("day2.txt")
        .map(l => {
          (l.split(" ").head,l.split(" ").apply(1))
        })
        .map{case (p1:String, p2: String) => (stringToPRSValue(p1), stringToResultValue(p2))}
        .map{case (p1:PRS.Value, p2: Results.Value) => (p1, forceResult(p1, p2))}
        .run(
          ZSink.foldLeft(0)(
            (incr, abc) => {
              abc match {
                case (p1,p2) => {
                  incr + p2.value + result(p1, p2).value
                }
                case _ => incr
              }
            })

        )
        .debug("Result")
    } yield ()
  }
}
