package Day2

import Day2.Results.{Draw, Lost, ResultsVal, Won}

object PRS extends Enumeration {
  protected case class PRSVal(value: Int, p1: Char, p2: Char) extends super.Val {
    def getValue: Int = value

    def getP1: Char = p1
    def getP2: Char = p2
  }
  import scala.language.implicitConversions
  implicit def valueToPRSVal(x: Value): PRSVal = x.asInstanceOf[PRSVal]

  def stringToPRSValue(x: String): PRSVal = {
    x match {
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissors
    }
  }

  val Rock: PRSVal = PRSVal(1, 'A', 'X')
  val Paper: PRSVal = PRSVal(2, 'B', 'Y')
  val Scissors: PRSVal = PRSVal(3, 'C', 'Z')

  def result (p1: PRSVal, p2: PRSVal) = {
    (p1, p2) match {
      case (Rock, Paper) => Won
      case (Rock, Rock) => Draw
      case (Rock, Scissors) => Lost

      case (Paper, Paper) => Draw
      case (Paper, Rock) => Lost
      case (Paper, Scissors) => Won

      case (Scissors, Paper) => Lost
      case (Scissors, Rock) => Won
      case (Scissors, Scissors) => Draw

    }
  }
}
