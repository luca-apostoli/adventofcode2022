package Day2

object Results extends Enumeration {
  protected case class  ResultsVal(value: Int) extends super.Val {
    def getValue: Int = value
  }

  val Won: ResultsVal = ResultsVal(6)
  val Lost: ResultsVal = ResultsVal(0)
  val Draw: ResultsVal = ResultsVal(3)

  def stringToResultValue(x: String): ResultsVal = {
    x match {
      case "X" => Lost
      case "Y" => Draw
      case "Z" => Won
    }
  }

  def forceResult(p1: PRS.Val, r: ResultsVal): PRS.Val = {
    (p1,r) match {
      case (PRS.Rock, Won) => PRS.Paper
      case (PRS.Paper, Won) => PRS.Scissors
      case (PRS.Scissors, Won) => PRS.Rock

      case (PRS.Rock, Draw) => PRS.Rock
      case (PRS.Paper, Draw) => PRS.Paper
      case (PRS.Scissors, Draw) => PRS.Scissors

      case (PRS.Rock, Lost) => PRS.Scissors
      case (PRS.Paper, Lost) => PRS.Rock
      case (PRS.Scissors, Lost) => PRS.Paper
    }
  }

}
