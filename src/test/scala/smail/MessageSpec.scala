package smail

import org.specs2.mutable.SpecificationWithJUnit
import scala.util.Failure
import org.parboiled2.ParseError
import scala.util.Success

class MessageSpec extends SpecificationWithJUnit {
  import Message._
  
  "MessageParser" >> {
    "Should parse simple address" >> {
      val parser = new Parser("foo@bar")
      parser.addrSpec.run() must beSuccessfulTry.withValue(Address.Mailbox("foo" -> "bar"))
    }
    
    "Should parse named address" >> {
      val parser = new Parser("foo <bar@baz>")
      parser.nameAddr.run().get === Address.Mailbox("bar" -> "baz", Some("foo"))
    }
    
    "Should parse group list" >> {
      val parser = new Parser("group: g1@d1.org, g2@d2.org;, group2: g3@d3.org;")
      parser.addressList.run().get === Seq(
        Address.Group("group", Seq(Address.Mailbox("g1" -> "d1.org"), Address.Mailbox("g2" -> "d2.org"))),
        Address.Group("group2", Seq(Address.Mailbox("g3" -> "d3.org")))
      )
    }
    
    "Should parse legacy display name" >> {
      val parser = new Parser("user@domain (Real Name)")
      parser.mailbox.run().get === Address.Mailbox("user" -> "domain", Some("Real Name"))
    }
    
    "Should parse special route address" >> {
      val parser = new Parser("<@route:user@domain>")
      parser.address.run().get === Address.Mailbox("user" -> "domain", None, Seq("route"))
    }
    
    "Should parse address with comments" >> {
      val parser = new Parser("<user (comment)@ (comment) domain.org>")
      parser.address.run().get === Address.Mailbox("user" -> "domain.org", None)
    }
    
    "Should parse in reply to field" >> {
      val parser = new Parser("In-Reply-To: <reply@to.id>\r\n")
      parser.inReplyTo.run().get === Message.Field.InReplyTo(Seq(Message.MsgId("reply", "to.id")))
    }
  }
}