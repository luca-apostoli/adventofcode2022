package Day11

import Utils.Utils.{iterate, lines}
import zio.Console.printLine
import zio.stream.ZSink
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object Main extends ZIOAppDefault {

  override def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] = {

    val m = for {
      _ <- printLine("Welcome to your Day11!")
      m <- lines("day11.txt")
        .run(ZSink.foldLeft(List.empty[String])((incr, o: String) => {
          incr.appended(o)
        }))
        .map(m => {
          m.grouped(7)
            .map(g => {
              val m = new Monkey(
                getMonkeyId(g.head),
                getOp(g.apply(2)),
                getDiv(g.apply(3)),
                getTestTrue(g.apply(4)),
                getTestFalse(g.apply(5))
              )
              m.setStartingItems(getStartingItems(g.apply(1)))
            })
            .toList
        })
        .debug("monkeys")
    } yield m

    for {
      res <- m
      mod : BigInt = res.foldLeft(BigInt.apply(1))((a, i) => {a * BigInt.apply(i.div)})
      _ <- iterate(Range.inclusive(0, 10000 - 1).toList)
        .run(ZSink.foldLeft(res)((incr, _) => {
          var l = incr
          for (m <- l.indices) {
            l = applyMonkey(l, m, mod)
          }
          l
        }))
        //        .map(m => {
        //          m.foreach(println(_))
        //        })
        .map(
          m => m.map(_.countItems).sorted(Ordering.BigInt.reverse).take(2).product
        )
        .debug("result")
    } yield ()
  }

  def applyMonkey(l: List[Monkey], i: Int, mod: BigInt): List[Monkey] = {
    var list = l
    var monkey = l.apply(i)
    for (j <- monkey.items) {
      monkey = monkey.incrementCount
      val w = (monkey.op(j) % mod)
      val t = monkey.test(w)
      list = list.updated(t, list.apply(t).addItem(w))
    }
    list.updated(i, monkey.setStartingItems(List.empty))
  }

  def getMonkeyId(s: String): Int = {
    MonkeyParser.parse(MonkeyParser.monkeyId, s) match {
      case MonkeyParser.Success(i, _) => i
    }
  }

  def getOp(s: String): (BigInt) => BigInt = {
    MonkeyParser.parse(MonkeyParser.oper, s) match {
      case MonkeyParser.Success(i, _) => i
    }
  }

  def getStartingItems(s: String): List[BigInt] = {
    MonkeyParser.parse(MonkeyParser.startingItems, s) match {
      case MonkeyParser.Success(i, _) => i
    }
  }

  def getDiv(s: String): Int = {
    MonkeyParser.parse(MonkeyParser.testDiv, s) match {
      case MonkeyParser.Success(i, _) => i
    }
  }

  def getTestTrue(s: String): Int = {
    MonkeyParser.parse(MonkeyParser.testTrue, s) match {
      case MonkeyParser.Success(i, _) => i
    }
  }

  def getTestFalse(s: String): Int = {
    MonkeyParser.parse(MonkeyParser.testFalse, s) match {
      case MonkeyParser.Success(i, _) => i
    }
  }

}
