package smail.imap

import org.specs2.mutable.SpecificationWithJUnit
import scala.util.Failure
import org.parboiled2.ParseError
import scala.util.Success

class ImapTokenParserSpec extends SpecificationWithJUnit {
  import ImapToken._
  
  "ImapTokenParser" >> {
    "Should parse simple authenticate request" >> {
      val parser = new ImapTokenParser("A0 AUTHENTICATE PLAIN")
      parser.Tokens.run() must beSuccessfulTry.withValue(Seq(Str("A0"), Str("AUTHENTICATE"), Str("PLAIN")))
    }
    
    "Should parse simple fetch request" >> {
      val parser = new ImapTokenParser("A3 FETCH 1 (BODY[]<0.16384>)")
      parser.Tokens.run() must beSuccessfulTry.withValue(
        Seq(Str("A3"), Str("FETCH"), Str("1"), List('(', Seq(Str("BODY"), List('[', Seq.empty), Str("<0.16384>"))))
      )
    }
    
    "Should parse simple list request" >> {
      val parser = new ImapTokenParser("A2 LIST \"\" \"%\"")
      parser.Tokens.run() must beSuccessfulTry.withValue(Seq(Str("A2"), Str("LIST"), Str("", true), Str("%", true)))
    }
    
    "Should parse str count prefix" >> {
      val parser = new ImapTokenParser("A4 APPEND /INBOX () {183}")
      parser.Tokens.run() must beSuccessfulTry.withValue(
        Seq(Str("A4"), Str("APPEND"), Str("/INBOX"), List('(', Seq()), StrCountPrefix(183))
      )
    }
    
    "Should parse raw str" >> {
      val str =
        "Message-ID: <1616438581.0.1436994826170@TESTS>\r\n" +
        "Subject: Append Test\r\n" +
        "MIME-Version: 1.0\r\n" +
        "Content-Type: text/plain; charset=us-ascii\r\n" +
        "Content-Transfer-Encoding: 7bit\r\n" +
        "\r\n" +
        "Append Test Body\r\n" +
        "And more info"
      val parser = new ImapTokenParser("{199}\r\n" + str)
      parser.Tokens.run() must beSuccessfulTry.withValue(Seq(Str(str)))
    }
    
    "Should parse flag request w/ backslash" >> {
      val parser = new ImapTokenParser("A4 STORE 14 +FLAGS (\\Seen)\r\n")
      parser.Tokens.run() must beSuccessfulTry.withValue(
        Seq(Str("A4"), Str("STORE"), Str("14"), Str("+FLAGS"), List('(', Seq(Str("\\Seen"))), Newline)
      )
    }
  }
}