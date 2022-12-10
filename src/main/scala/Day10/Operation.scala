package Day10

object Operation extends Enumeration {
  protected case class OPVal(value: Int, target: Option[Int]) extends super.Val {
    def getValue: Int = value
  }

  def Noop: OPVal = OPVal(0, None)
  def Addx(x: Int): OPVal = OPVal(1, Some(x))

  def stringToOPValue(x: String, target: Option[Int]): OPVal = {
    x match {
      case "noop" => Noop
      case "addx" => {
        target match {
          case Some(t) => Addx(t)
          case None => Addx(0)
        }
      }
    }

  }

}


