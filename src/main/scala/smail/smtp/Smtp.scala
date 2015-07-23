package smail.smtp

object Smtp {
  case class Address(local: String, domain: String)
  
  case class Failure(code: ReplyCode, contents: String)
  object Failure {
    def apply(code: ReplyCode): Failure = Failure(code, code.defaultString)
    def apply(code: Int, contents: String): Failure = Failure(_replyCodes(code), contents)
    def apply(code: Int): Failure = Failure(_replyCodes(code))
  }
  
  sealed trait ReplyCodeType
  case object PositiveCompletionReply extends ReplyCodeType
  case object PositiveIntermediateReply extends ReplyCodeType
  case object TransitiveNegativeCompletionReply extends ReplyCodeType
  case object PositiveNegativeCompletionReply extends ReplyCodeType
  def ReplyCodeType(chr: Char): ReplyCodeType = chr match {
    case '2' => PositiveCompletionReply
    case '3' => PositiveIntermediateReply
    case '4' => TransitiveNegativeCompletionReply
    case '5' => PositiveNegativeCompletionReply
    case _ => sys.error("Unrecognized char: " + chr)
  }
  
  sealed trait ReplyCategory
  case object SyntaxCategory extends ReplyCategory
  case object InformationCategory extends ReplyCategory
  case object ConnectionsCategory extends ReplyCategory
  case object MailSystemCategory extends ReplyCategory
  def ReplyCategory(chr: Char): ReplyCategory = chr match {
    case '0' => SyntaxCategory
    case '1' => InformationCategory
    case '2' => ConnectionsCategory
    case '5' => MailSystemCategory
    case _ => sys.error("Unrecognized char: " + chr)
  }
  
  case class ReplyCode(typ: ReplyCodeType, cat: ReplyCategory, extra: Char, defaultString: String = "")
  
  private var _replyCodes = Map.empty[Int, ReplyCode]
  private def addReplyCode(int: Int, defaultString: String): ReplyCode = {
    val str = int.toString
    require(str.length == 3)
    val ret = ReplyCode(ReplyCodeType(str(0)), ReplyCategory(str(1)), str(2), defaultString)
    _replyCodes += int -> ret
    ret
  }
  def replyCodes: Map[Int, ReplyCode] = _replyCodes
  
  object ReplyCode {
    val ServiceReady = addReplyCode(220, "Service ready")
    val Ok = addReplyCode(250, "Requested mail action okay, completed")
    val StartMailInput = addReplyCode(354, "Start mail input; end with <CRLF>.<CRLF>")
  }
}