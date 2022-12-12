package Day9

import Utils.Utils.lines
import zio.Console.printLine
import zio.stream.ZSink
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Main extends ZIOAppDefault {

  override def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] = {

    var hPos: (Int, Int) = (0, 0)
    var tPos: List[(Int, Int)] = List.fill(10)(0, 0)
    var ts : Set[(Int, Int)] = Set.empty

    for {
      _ <- printLine("Welcome to your Day9!")
      _ <- lines("day9.txt")
        .map(s => {
          val m = s.split(" ")
          m.head match {
            case "U" => ((x: (Int, Int)) => (x._1, x._2 + m.last.toInt))
            case "R" => ((x: (Int, Int)) => (x._1 + m.last.toInt , x._2))
            case "L" => ((x: (Int, Int)) => (x._1 - m.last.toInt , x._2))
            case "D" => ((x: (Int, Int)) => (x._1, x._2 - m.last.toInt  ))
          }
        })
        .run(ZSink.foldLeft(1)((incr, o) => {
          val hpos2 = o(hPos)
          val l = moveHead(hPos, hpos2, List.empty[(Int,Int)])

          tPos = tPos.updated(0, hpos2)
          hPos = hpos2
          for(h <- l) {
            for (i <- 1 until  tPos.length) {
              val tPos2 = moveTail(tPos.apply(i - 1), tPos.apply(i))
              if (i == tPos.length - 1) {
                ts = ts.incl(tPos2)
              }
              tPos = tPos.updated(i, tPos2)
            }
          }
          ts.size
        })
        ).debug("result")
    } yield ()
  }

  def moveHead(h: (Int, Int), h2: (Int, Int), l : List[(Int, Int)]): List[(Int, Int)] = {
    if (h == h2) {
      return l.appended(h)
    }
    var list = l
    if (h._1 < h2._1) {
      list = moveHead((h._1 + 1, h._2), h2, l.appended(h))
    } else if (h._1 > h2._1) {
      list = moveHead((h._1 - 1, h._2), h2, l.appended(h))
    } else if (h._2 > h2._2) {
      list = moveHead((h._1, h._2 - 1), h2, l.appended(h))
    } else if (h._2 < h2._2) {
      list = moveHead((h._1, h._2 + 1), h2, l.appended(h))
    }
    list
  }

  def moveTail(h: (Int, Int), t: (Int, Int)): (Int, Int) = {
    // overlap
    if (h == t || isAdjacent(h, t)) {
      t
    } else if (h._1 == t._1) { //same row
      if (h._2 > t._2) {
        (t._1, t._2 + 1)
      } else {
        (t._1, t._2 - 1)
      }
    } else if (h._2 == t._2) { //same col
      if (h._1 > t._1) {
        (t._1 + 1, t._2)
      } else {
        (t._1 - 1, t._2)
      }
    } else {
      var newt1 : Int = t._1
      var newt2 : Int = t._2
      if (h._1 > t._1) {
        newt1 = t._1 + 1
      } else {
        newt1 = t._1 - 1
      }

      if (h._2 > t._2) {
        newt2 = t._2 + 1
      } else {
        newt2 = t._2 - 1
      }
      (newt1, newt2)
    }
  }

  def isAdjacent(h: (Int, Int), t: (Int, Int)) : Boolean = {
    if (h == t) {
      true
    } else if (h._1 == t._1 && (t._2 == (h._2 -1) || t._2 == (h._2 + 1))) { //same row
      true
    } else if (h._2 == t._2 && (t._1 == (h._1 - 1) || t._1 == (h._1 + 1))) { //same col
     true
    } else if ((t._2 == (h._2 -1) || t._2 == (h._2 + 1)) && (t._1 == (h._1 - 1) || t._1 == (h._1 + 1))) { // diag
      true
    } else {
      false
    }
  }

}
