package scimap.smtp

sealed trait ClientCommand
object ClientCommand {
  
  case class ExtendedHello(address: Option[String]) extends ClientCommand
  case class Hello(domain: String) extends ClientCommand
  
  case class Mail(mailbox: Option[Smtp.Address], params: Seq[(String, String)]) extends ClientCommand
  
  case class Recipient(path: RecipientPath, params: Seq[(String, String)]) extends ClientCommand
  sealed trait RecipientPath
  object RecipientPath {
    case object Postmaster extends RecipientPath
    case class PostmasterDomain(domain: String) extends RecipientPath
    case class Path(address: Smtp.Address) extends RecipientPath
  }

  case object Data extends ClientCommand
  case class DataLine(line: String) extends ClientCommand
  case object DataEnd extends ClientCommand
  
  case object Reset extends ClientCommand
  
  case class Verify(string: String) extends ClientCommand
  
  case class Expand(string: String) extends ClientCommand
  
  case class Help(string: String) extends ClientCommand
  
  case class Noop(string: String) extends ClientCommand
  
  case object Quit extends ClientCommand
}