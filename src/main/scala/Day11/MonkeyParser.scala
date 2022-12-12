package Day11

import scala.util.parsing.combinator.RegexParsers

object MonkeyParser extends RegexParsers{
  def monkeyId: Parser[Int] = "Monkey [0-9]+:".r ^^ {
    s => s.replace("Monkey","").replace(':', ' ').trim.toInt
  }
  def startingItems: Parser[List[BigInt]] = "Starting items: [0-9,\\s]+".r ^^ {
    s => s.replace("Starting items:","").trim().split(",").map(x => BigInt.apply(x.trim)).toList
  }

  def oper: Parser[(BigInt) => BigInt] = "Operation: new = old [\\*\\+]+ [0-9old]+".r ^^ {
    s => {
      val c = s.replace("Operation: new = old", "").trim.split(" ")
      if (c.last == "old") {
        if (c.head == "*") {
          (x: BigInt) => {x * x}
        } else {
          (x: BigInt) => {x + x}
        }
      } else {
        val d = c.last.trim.toInt
        if (c.head == "*") {
          (x: BigInt) => {x * d}
        } else {
          (x: BigInt) => { x + d }
        }
      }
    }
  }

  def testDiv: Parser[Int] = "Test: divisible by [0-9]+".r ^^ {
    s => s.replace("Test: divisible by", "").trim.toInt
  }
  def testTrue: Parser[Int] = "If true: throw to monkey [0-9]+".r ^^ {
    s => s.replace("If true: throw to monkey", "").trim.toInt
  }
  def testFalse: Parser[Int] = "If false: throw to monkey [0-9]+".r ^^ {
    s => s.replace("If false: throw to monkey", "").trim.toInt
  }
}
