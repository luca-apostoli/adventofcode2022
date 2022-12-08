package Day7

import scala.util.parsing.combinator.RegexParsers

object OSParser extends RegexParsers{
  def command: Parser[Command] = "^\\$ (cd .+|ls)".r ^^ {
    s => {
      val words = s.replace('$', ' ').trim.split(" ")
      Command(words.head, Option.when(words.length > 1)(words.drop(1).head))
    }
  }

  def file: Parser[OSElement] = "(dir [a-z]+|[0-9]+ [a-z\\.]+)".r ^^ {
    s => {
      val l = s.split(" ")
      if(l.head == "dir") {
        OSElement(l.drop(1).head, None)
      } else {
        OSElement(l.drop(1).head, Some(l.head.toInt))
      }
    }
  }

  def os: Parser[Product] = file | command

}

case class OSElement(var name: String, var size: Option[Int]) extends Product {}
