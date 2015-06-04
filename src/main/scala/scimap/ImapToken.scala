package scimap

sealed trait ImapToken

object ImapToken {
  case object Newline extends ImapToken
  case class Str(value: String, quoted: Boolean = false) extends ImapToken
  case class List(char: Char, values: Seq[ImapToken]) extends ImapToken
  case class StrCountPrefix(amount: Int) extends ImapToken
  val Nil = Str("Nil")
}