package scimap

import org.parboiled2._
import akka.util.ByteString

class ImapTokenParser(val input: ParserInput) extends Parser with StringBuilding {
  def Tokens: Rule1[Seq[ImapToken]] = rule {
    optional(Ws) ~ zeroOrMore(Token ~ optional(Ws)) ~ optional(Ws)
  }
  
  def Token = rule {
    ParensList | BracketList | Str | QuotedStr | CountPrefixedStr | CountPrefix | Newline
  }
  
  def Str = rule { capture(oneOrMore(!AtomSpecials ~ ANY)) ~> (ImapToken.Str(_)) }
  
  def QuotedStr = rule {
    '"' ~ clearSB ~ zeroOrMore(QuotedChar) ~ '"' ~ push(sb.toString) ~> (ImapToken.Str(_, true))
  }
  
  def QuotedChar = rule { ("\\\\" | "\\\"" | (!ch('"') ~ ANY)) ~ appendSB }
  
  def CountPrefixedStr: Rule1[ImapToken.Str] = rule {
    CountPrefix ~
      str("\r\n") ~
      capture(run((prefix: ImapToken.StrCountPrefix) => rule { prefix.amount.times(ANY) })) ~>
      (ImapToken.Str(_: String))
  }
  
  def CountPrefix = rule {
    '{' ~ capture(oneOrMore(CharPredicate.Digit)) ~ '}' ~> ((str: String) => ImapToken.StrCountPrefix(str.toInt))
  }
  
  def CountPrefixString = rule { '{' ~ capture(oneOrMore(CharPredicate.Digit)) ~ '}' }
  
  def Newline = rule { str("\r\n") ~> (() => ImapToken.Newline) }
  
  def Ws = rule { ch(' ') | '\t' }
  
  def ParensList = rule { '(' ~ Tokens ~ ')' ~> (ImapToken.List('(', _)) }
  
  def BracketList = rule { '[' ~ Tokens ~ ']' ~> (ImapToken.List('[', _)) }
  
  def ListWildcards = rule { ch('%') | '*' }
  
  // This was removed because it skipped over flags like \Seen
  // def QuotedSpecials = rule { ch('"') | ('\\' }
  def QuotedSpecials = rule { ch('"') | "\\\"" | "\\\\" }
  
  // Note, added extra '[' because I want the list to capture it
  def RespSpecials = rule { ch('[') | ']' }
  
  def AtomSpecials = rule {
    ImapTokenParser.ControlChar | '(' | ')' | '{' | ' ' | ListWildcards | QuotedSpecials | RespSpecials
  }
}

object ImapTokenParser {
  val ControlChar = CharPredicate('\u0000' to '\u001f') ++ '\u007e'
  
  class BufferedParserInput(val buffer: StringBuilder) extends ParserInput.DefaultParserInput {
    override def charAt(ix: Int): Char = buffer.charAt(ix)
    override def length: Int = buffer.length
    override def sliceString(start: Int, end: Int): String = buffer.substring(start, end)
    override def sliceCharArray(start: Int, end: Int): Array[Char] = sliceString(start, end).toArray
  }
}