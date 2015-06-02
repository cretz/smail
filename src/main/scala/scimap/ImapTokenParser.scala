package scimap

import org.parboiled2._
import akka.util.ByteString

class ImapTokenParser(val input: ParserInput) extends Parser with StringBuilding {
  def Tokens: Rule1[Seq[ImapToken]] = rule {
    optional(Ws) ~ zeroOrMore(Token ~ optional(Ws))
  }
  
  def Token = rule {
    Str | QuotedStr | CountPrefixedStr | CountPrefix | Newline | ParensList | BracketList
  }
  
  def Str = rule { capture(oneOrMore(!TopLevelInvalid ~ ANY)) ~> (ImapToken.Str(_)) }
  
  def QuotedStr = rule {
    '"' ~ clearSB ~ zeroOrMore(QuotedChar) ~ '"' ~ push(sb.toString) ~> (ImapToken.Str(_))
  }
  
  def QuotedChar = rule { ("\\\\" | "\\\"" | !ch('\\') | ANY) ~ appendSB }
  
  def CountPrefixedStr = rule {
    '{' ~ capture(oneOrMore(CharPredicate.Digit)) ~ '}' ~ valueStack.pop.toString.toInt.times(ANY) ~>
      (ImapToken.Str(_))
  }
  
  def CountPrefix = rule {
    '{' ~ capture(oneOrMore(CharPredicate.Digit)) ~ '}' ~> ((str: String) => ImapToken.StrCountPrefix(str.toInt))
  }
  
  def CountPrefixString = rule { '{' ~ capture(oneOrMore(CharPredicate.Digit)) ~ '}' }
  
  def Newline = rule { str("\r\n") ~> (() => ImapToken.Newline) }
  
  def Ws = rule { ch(' ') | '\t' }
  
  def TopLevelInvalid = rule { ImapTokenParser.ControlChar | '(' | '{' | '[' | ' ' | '"' }
  
  def ParensList = rule { '(' ~ Tokens ~ ')' ~> (ImapToken.List('(', _)) }
  
  def BracketList = rule { '[' ~ Tokens ~ ']' ~> (ImapToken.List('[', _)) }
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