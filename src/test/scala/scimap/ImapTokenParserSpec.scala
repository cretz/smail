package scimap

import org.specs2.mutable.SpecificationWithJUnit

class ImapTokenParserSpec extends SpecificationWithJUnit {
  import ImapToken._
  
  "ImapTokenParser" >> {
    "Should parse simple authenticate request" >> {
      val parser = new ImapTokenParser("A0 AUTHENTICATE PLAIN")
      parser.Tokens.run() must beSuccessfulTry.withValue(Seq(Str("A0"), Str("AUTHENTICATE"), Str("PLAIN")))
    }
  }
}