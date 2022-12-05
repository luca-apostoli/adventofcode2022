package Day5

import Utils.Utils.{lines, raw}
import zio.Console.printLine
import zio.stream.ZSink
import zio.{Promise, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

import scala.language.postfixOps

object Main extends ZIOAppDefault {

  def cargo: ZIO[Any, Throwable, List[Seq[Cargo]]] = {
    for {
      l <- raw("day5.txt")
        .map(
          x =>
            CargoParser.parse(CargoParser.row, x) match {
              case CargoParser.Success(d, _) => Some(d)
              case CargoParser.Failure(_, _) => None
          }
        )
        .filter { case None => false; case Some(_) => true }
        .map {
          case Some(a) => a
        }
        .filter{ l => !l.forall(_.isEmpty) }
        .run(ZSink.foldLeft(List.empty[Seq[Cargo]])((a, s) => {
          var b = a
          s.foreach(_ => {
            b = b.appended(Seq.empty[Cargo])
          })
          s.indices.foldLeft(b)((x, i) => {
            val n = s.apply(i).toList
            if (n.nonEmpty) {
              x.updated(i, x.apply(i).appendedAll(n))
            } else {
              x
            }
          })
        }))
        .debug("board")
    } yield l
  }

  override def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] = {

    for {
      p <- Promise.make[Any, List[Seq[Cargo]]]
      _ <- p.completeWith(cargo)
      _ <- p.await.flatMap(
        board =>
          for {
            _ <- printLine("Welcome to your Day5!")
            _ <- lines("day5.txt")
              .map { x =>
                MoveParser.parse(MoveParser.move, x) match {
                  case MoveParser.Success(d, _) => Some(d)
                  case MoveParser.Failure(_, _) => None
                }
              }
              .filter { case None => false; case Some(_) => true }
              .run(ZSink.foldLeft(board)((r, m) => {
                m match {
                  case Some(mov) => applyMove(mov, r)
                  case _         => r
                }
              })
              ).map(_.map{x => if (x.nonEmpty) { x.head } else { "" }})
              .debug("Result")
          } yield ()
      )
    } yield ()
  }

  def applyMove(m: Move, n: List[Seq[Cargo]]): List[Seq[Cargo]] = {
    var l = n
    val tmp = l.apply(m.getFrom).take(m.num)
    l = l.updated(m.getTo, tmp.appendedAll(l.apply(m.getTo)))
    l = l.updated(m.getFrom, l.apply(m.getFrom).drop(m.num))
    l
  }

}
