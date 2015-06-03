package scimap

import org.specs2.mutable.SpecificationWithJUnit
import scimap.JavaMailMemoryServer.Context
import javax.mail.Folder
import scimap.handler.InMemoryServer

class JavaMailSpec extends SpecificationWithJUnit with JavaMailMemoryServer {
  "JavaMail API" >> {
    
    "Should be able to fetch simple messages" >> { ctx: Context =>
      ctx.server.users += createTestUser
//      ctx.handler.users +=
      println("Connecting")
      ctx.store
      println("Asking for inbox")
      val inbox = ctx.store.getFolder("INBOX")
      println("Opening inbox")
      inbox.open(Folder.READ_ONLY)
      println("Getting messages")
      val msgs = inbox.getMessages
      println("Got messages")
      msgs.foreach { msg =>
        println("MSG!")
        System.out.println("---------\n" + msg.getContent + "\n---------")
      }
//      try {
//        
//      } finally inbox.close(false)
      1 === 1
    }
  }
    
  def createTestUser() = {
    import InMemoryServer._
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
              headers = Map("Subject" -> "Test 1"),
              body = "Test message 1"
            ),
            new InMemoryMessage(
              uid = 2,
              flags = Set(Imap.Flag.Seen),
              headers = Map("Subject" -> "Test 2"),
              body = "Test message 2"
            ),
            new InMemoryMessage(
              uid = 3,
              flags = Set(Imap.Flag.Recent),
              headers = Map("Subject" -> "Test 3"),
              body = "Test message 3"
            )
          )
        )
      )
    )
  }
}