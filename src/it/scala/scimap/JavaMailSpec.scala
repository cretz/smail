package scimap

import org.specs2.mutable.SpecificationWithJUnit
import scimap.JavaMailMemoryServer.Context
import javax.mail.Folder
import scimap.handler.InMemoryServerHandler
import scala.collection.SortedMap

class JavaMailSpec extends SpecificationWithJUnit with JavaMailMemoryServer {
  "JavaMail API" >> {
    
    "Should be able to fetch simple messages" >> { ctx: Context =>
      ctx.handler.users += createTestUser
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
//      try {
//        
//      } finally inbox.close(false)
      1 === 1
    }
  }
    
  def createTestUser() = {
    import InMemoryServerHandler._
    "foo" -> new User(
      username = "foo",
      password = "bar",
      mailboxes = Map(
        "INBOX" -> new Mailbox(
          name = "INBOX",
          flags = Set(Imap.Flag.Answered),
          permanentFlags = Set.empty,
          messages = SortedMap(
            BigInt(1) -> new Message(
              uid = 1,
              flags = Set.empty
            ),
            BigInt(2) -> new Message(
              uid = 2,
              flags = Set(Imap.Flag.Seen)
            ),
            BigInt(3) -> new Message(
              uid = 3,
              flags = Set(Imap.Flag.Recent)
            )
          )
        )
      )
    )
  }
}