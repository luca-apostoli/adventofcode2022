package Day6

import zio.Console.printLine
import Utils.Utils.{lines, row}
import zio.stream.ZSink
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Main extends ZIOAppDefault {

  override def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] = {
    for {
      _ <- printLine("Welcome to your Day6!")
      _ <- row("day6.txt")
        .run(ZSink.foldLeft((Seq.empty[String], Left[Int, Int](0).withRight[Int]))((incr, a) => {
          incr match {
            case (s, Left(c)) => {
              if (s.size < 14) {
                (s.appended(a), Left[Int, Int](c + 1))
              } else {
                if (s.contains(a)) {
                  (s.drop(1).appended(a), Left[Int, Int](c + 1))
                } else {
                  if (s.diff(s.distinct).distinct.isEmpty) {
                    (s, Right[Int, Int](c))
                  } else {
                    (s.drop(1).appended(a), Left[Int, Int](c + 1))
                  }
                }
              }

            }
            case (s, Right(c)) => (s, Right[Int, Int](c).withLeft)
          }

        }))
        .debug("Result")
    } yield ()
  }
}
