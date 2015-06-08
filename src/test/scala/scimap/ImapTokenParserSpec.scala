package scimap

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
      1 === 1
    }
  }
}