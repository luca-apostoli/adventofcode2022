package Day5

import scala.util.parsing.combinator.RegexParsers

object CargoParser extends RegexParsers{

  override def skipWhitespace = false

  def cname: Parser[String] = {
    "\\[[A-Z]+\\]".r ^^ { s => s.substring(1, s.length - 1) }
  }

  def row: Parser[List[Seq[Cargo]]] = {
    "[\\[A-Z\\]\\s]+".r ^^ {
      s => s.grouped(4).foldLeft((List.empty[Seq[Cargo]], 0))((a, b) => {
        a match {
          case (lst, num) => {
            var row = lst
            if (!lst.isDefinedAt(num)) {
              row = lst.appended(Seq.empty[Cargo])
            }
            if (b.isEmpty || b.isBlank || b.contentEquals("   ")) {
              (row, num + 1)
            } else {

              val c = new Cargo(parse(cname, b).getOrElse(""), num + 1)
              (row.updated(num, row.apply(num).appended(c)), num + 1)
            }
          }
        }
      })._1
    }
  }
}

class Cargo(var name: String, var ord: Int) {
  override def toString: String = {
    name
  }
}
