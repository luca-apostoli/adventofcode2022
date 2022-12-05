package Day5

import scala.util.parsing.combinator.RegexParsers

object MoveParser extends RegexParsers{
  def num: Parser[Int] = {
    "move ([0-9]+)".r ^^ { s => s.substring("move".length).trim.toInt }
  }

  def from: Parser[Int] = {
    "from ([0-9]+)".r ^^ { s => s.substring("from".length).trim.toInt }
  }

  def to: Parser[Int] = {
    "to ([0-9]+)".r ^^ { s => s.substring("to".length).trim.toInt }
  }
  def move: Parser[Move] = {
    num ~ from ~ to ^^ { case n ~ f ~ t =>
      new Move(n,f,t)
    }
  }
}

class Move(var num: Int, var from: Int, var to: Int) {
  override def toString = "move " + num.toString + " from " + from.toString + " to " + to.toString

  def getTo : Int = to - 1
  def getFrom : Int = from - 1
}


