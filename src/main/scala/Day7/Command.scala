package Day7

case class Command (var cmd: String, var param: Option[String]) {
}

object Cmd {
  val CDBack: Command = Command("cd", Some(".."))
  val CDIn: String => Command = (x: String) => Command("cd", Some(x))
  val CDRoot: Command = Command("cd", Some("/"))
  val LS: Command = Command("ls", None)
}