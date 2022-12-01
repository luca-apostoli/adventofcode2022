package Day1

import zio.Console.printLine
import Utils.Utils.lines
import zio.stream.ZSink
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Main extends ZIOAppDefault {

  override def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] = {
    for {
      _ <- printLine("Welcome to your Day1!")
      _ <- lines("day1.txt")
        .run(
          ZSink.foldLeft(0 :: List.empty, 0)(
            (incr, abc) => {
              incr match {
                case (maxl, curr) => {
                  var _curr = curr
                  var _max = maxl
                  if (abc.isEmpty) {
                    _curr = 0
                    _max = _max.appended(curr)
                  } else {
                    _curr = curr + abc.toInt
                  }
                  (_max, _curr)
                }
              }
            })

        ).map { case (b: List[Int], _) => b.sorted(Ordering.Int.reverse).take(3).sum }
        .debug("Result")
    } yield ()
  }
}
