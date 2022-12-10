package Day10


import Utils.Utils.lines
import zio.Console.printLine
import zio.stream.ZSink
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Main extends ZIOAppDefault {

  override def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] = {
    val zero = List.empty[(Int, Int)].appended((1,0))
    for {
      _ <- printLine("Welcome to your Day10!")
      _ <- lines("day10.txt")
        .map(s => {
          val s1 = s.split(" ")
          Operation.stringToOPValue(s1.head, Option.when(s1.length > 1)(s1.drop(1).head.toInt))
        })
        .run(ZSink.foldLeft(zero)((incr, o) => {
          val x = incr.last
          if (o.value == 0) {
              incr.appended((x._1, x._2 +1))
          } else {
              var i = incr
              for (c <- 0 until   o.value) {
                i = i.appended((x._1, x._2 + c + 1))
              }
              i = i.appended((x._1 + o.target.getOrElse(0), x._2 + o.value + 1))
              i
          }
        }))
        .map(i => i.map(_._1).grouped(40).map(x => {
          var c : List[Char] = List.empty
          for(i <- x.indices) {
            if(i >= (x.apply(i) - 1) && i <= (x.apply(i) + 1)) {
              c = c.appended('#')
            } else {
              c = c.appended('.')
            }
          }
          c
        }).foreach(s => println(s)))


//        /*
//   PART ONE
//
//        .map(r => {
//          val l : List[Int] = (20, 60, 100, 140, 180, 220)
//            .productIterator.toList.map(_.toString
//            .toInt)
//
//          l.map(i => r.apply(i - 1 ) * i)
//        })
//        .map(m => m.sum)
//
//  **/
        .debug("Result")
    } yield ()
  }
}
