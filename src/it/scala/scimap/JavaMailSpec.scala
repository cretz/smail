package scimap

import org.specs2.mutable.SpecificationWithJUnit
import scimap.JavaMailMemoryServer.Context
import javax.mail.Folder
import scimap.handler.InMemoryServer
import java.time.ZonedDateTime

class JavaMailSpec extends SpecificationWithJUnit with JavaMailMemoryServer {
  "JavaMail API" >> {
    
    "Should be able to fetch simple messages" >> { ctx: Context =>
      ctx.server.users += createTestUser
      val inbox = ctx.store.getFolder("INBOX")
      inbox.open(Folder.READ_ONLY)
      inbox.getMessages.headOption.map(msg => msg.getSubject -> msg.getContent) ===
        Some("Test 1" -> "Test message 1")
//      inbox.getMessages.map(msg => msg.getSubject -> msg.getContent) ===
//        Seq("Test 1" -> "Test message 1", "Test 2" -> "Test message 2", "Test 3" -> "Test message 3")
    }
  }
    
  def createTestUser() = {
    import InMemoryServer._
    import MailHeaders._
    "foo" -> new InMemoryUser(
      username = "foo",
      password = "bar",
      mailboxes = Map(
        "INBOX" -> new InMemoryMailbox(
          name = "INBOX",
          flags = Set(Imap.Flag.Answered),
          permanentFlags = Set.empty,
          messages = Seq(
            new InMemoryMessage(
              uid = 1,
              flags = Set.empty,
              headers = InMemory() +
                (Subject -> "Test 1") +
                (From -> Seq(Imap.MailboxAddress("foo" -> "bar"))) +
                (OrigDate -> ZonedDateTime.now()) +
                (MessageId -> ("foo123" -> "bar123")) +
                (To -> Seq(Imap.MailboxAddress("baz" -> "qux"))),
              body = "Test message 1"
            )/*,
            new InMemoryMessage(
              uid = 2,
              flags = Set(Imap.Flag.Seen),
              headers = InMemory() + (Subject -> "Test 2"),
              body = "Test message 2"
            ),
            new InMemoryMessage(
              uid = 3,
              flags = Set(Imap.Flag.Recent),
              headers = InMemory() + (Subject -> "Test 3"),
              body = "Test message 3"
            )*/
          )
        )
      )
    )
  }
}