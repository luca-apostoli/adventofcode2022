package Day4

import scala.util.parsing.combinator.RegexParsers

object RangeParser extends RegexParsers{
  def range : Parser[Range] = {
    "[0-9]+\\-[0-9]+".r ^^ { str =>
      Range.inclusive(
        str.split("-").head.toInt,
        str.split("-").apply(1).toInt) }
  }
}
