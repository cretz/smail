package scimap

import scala.collection.JavaConversions._
import org.specs2.mutable.SpecificationWithJUnit
import scimap.JavaMailMemoryServer.Context
import javax.mail.Folder
import scimap.handler.InMemoryServer
import java.time.ZonedDateTime
import javax.mail.internet.InternetAddress
import javax.mail.Flags
import javax.mail.Header
import com.sun.mail.imap.IMAPMessage
import javax.net.ssl.SSLContext
import java.security.KeyStore
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.TrustManagerFactory
import java.security.SecureRandom
import javax.mail.event.MessageCountListener
import javax.mail.event.MessageCountEvent
import scala.concurrent.Future
import scala.concurrent.duration._
import org.specs2.concurrent.ExecutionEnv
import javax.mail.UIDFolder
import com.sun.mail.imap.IMAPFolder

class JavaMailSpec extends SpecificationWithJUnit with JavaMailMemoryServer {
  sequential
  
  type EE = ExecutionEnv
  
  "JavaMail API" >> {

    "Should be able to fetch simple messages" >> { ctx: Context =>
      ctx.server.users += createTestUser
      ctx.daemon.start()
      val inbox = ctx.store.getFolder("INBOX")
      inbox.open(Folder.READ_ONLY)
      val msgs = inbox.getMessages
      msgs.length === 30
      val secondMsg = msgs(1)
      // At first it's unseen
      secondMsg.getFlags === new Flags()
      secondMsg.getAllRecipients.toSeq === Seq(new InternetAddress("baz@qux"))
      secondMsg.getContent === "Test message 2"
      // Make sure it's now considered "seen"
      secondMsg.asInstanceOf[IMAPMessage].invalidateHeaders()
      secondMsg.getFlags === new Flags(Flags.Flag.SEEN)
      secondMsg.getFrom.toSeq === Seq(new InternetAddress("foo@bar"))
      secondMsg.getMessageNumber === 2
      // No ms in imap
      val headers = ctx.server.currentMailbox.get.messages(1).headers
      secondMsg.getReceivedDate.getTime === headers(MailHeaders.Date).get.toEpochSecond * 1000
      // For now I reuse this
      secondMsg.getSentDate === secondMsg.getReceivedDate
      secondMsg.getSubject === "Test 2"
      secondMsg.getAllHeaders.map(_.asInstanceOf[Header]).map(h => h.getName -> h.getValue).toSet ===
        headers.headers.keys.flatMap(headers.lines(_)).map(_.split(": ", 2) match { case Array(k, v) => k -> v }).toSet
      secondMsg.getContentType === "TEXT/PLAIN; charset=US-ASCII"
      Option(secondMsg.getDescription) === None
      Option(secondMsg.getDisposition) === None
      Option(secondMsg.getFileName) === None
      secondMsg.getHeader(MailHeaders.MessageId.name).toSeq === Seq("<foo2@bar2>")
      secondMsg.getLineCount === 1
      secondMsg.getSize === 14
    }
    
    "Should list properly" >> { ctx: Context =>
      // We try to hit the commands we didn't in the previous test
      val (testUsername, testUser) = createTestUser
      ctx.server.users += testUsername -> testUser
      ctx.daemon.start()
      // Let's see the children of the default folder
      val defaultFolder = ctx.store.getDefaultFolder
      defaultFolder.getName === ""
      val folders = defaultFolder.list
      folders.length === 1
      folders(0).getName === "INBOX"
      // Open the inbox in read-write for fun...
      val inbox = ctx.store.getFolder("INBOX")
      inbox.open(Folder.READ_WRITE)
      inbox.getMessages.length === 30
      // TODO: Namespaces...
      // val namespaces = ctx.store.getPersonalNamespaces
    }
    
    "Should handle non-idle count update" >> { implicit ee: EE => ctx: Context =>
      val (testUsername, testUser) = createTestUser
      ctx.server.users += testUsername -> testUser
      ctx.daemon.start()
      val inbox = ctx.store.getFolder("INBOX")
      inbox.open(Folder.READ_WRITE)
      inbox.getMessages.length === 30
      @volatile var countEvents = Seq.empty[MessageCountEvent]
      inbox.addMessageCountListener(new MessageCountListener {
        override def messagesAdded(e: MessageCountEvent) = countEvents :+= e
        override def messagesRemoved(e: MessageCountEvent) = countEvents :+= e
      })
      // Add a message
      testUser.mailboxes.values.head.messages :+= newMessage(3000)
      // We have to wait over a second because of Java IMAP impl
      Thread.sleep(1100)
      // Fetch message count to trigger listener
      inbox.getMessageCount()
      countEvents.size must be_==(1).eventually
      countEvents(0).getType === MessageCountEvent.ADDED
      countEvents(0).getMessages.length === 1
      val msg = countEvents(0).getMessages()(0)
      msg.getMessageNumber === 31
      inbox.asInstanceOf[UIDFolder].getUID(msg) === 3000
      msg.getSubject === "Test 3000"
      msg.getContent === "Test message 3000"
    }
    
      // TODO: this
//    "Should handle idle count update" >> { implicit ee: EE => ctx: Context =>
//      val (testUsername, testUser) = createTestUser
//      ctx.server.users += testUsername -> testUser
//      ctx.server._capabilities :+= Imap.Capability.Idle
//      ctx.daemon.start()
//      val inbox = ctx.store.getFolder("INBOX")
//      inbox.open(Folder.READ_WRITE)
//      inbox.getMessages.length === 30
//      @volatile var countEvents = Seq.empty[MessageCountEvent]
//      inbox.addMessageCountListener(new MessageCountListener {
//        override def messagesAdded(e: MessageCountEvent) = countEvents :+= e
//        override def messagesRemoved(e: MessageCountEvent) = countEvents :+= e
//      })
//      // Put into idle to get updates
//      inbox.asInstanceOf[IMAPFolder].idle()
//      // Add a message
//      testUser.mailboxes.values.head.messages :+= newMessage(3000)
//      countEvents.size must be_==(1).eventually
//      countEvents(0).getType === MessageCountEvent.ADDED
//      countEvents(0).getMessages.length === 1
//      val msg = countEvents(0).getMessages()(0)
//      msg.getMessageNumber === 31
//      inbox.asInstanceOf[UIDFolder].getUID(msg) === 3000
//      msg.getSubject === "Test 3000"
//      msg.getContent === "Test message 3000"
//    }

    "Should be able to handle TLS" >> { ctx: Context =>
      ctx.useTls()
      ctx.server.users += createTestUser
      ctx.daemon.start()
      val inbox = ctx.store.getFolder("INBOX")
      inbox.open(Folder.READ_WRITE)
      val msgs = inbox.getMessages
      msgs.length === 30
      val secondMsg = msgs(1)
      secondMsg.getFlags === new Flags()
      secondMsg.getAllRecipients.toSeq === Seq(new InternetAddress("baz@qux"))
    }
  }
  
  def newMessage(uid: Int) = {
    import InMemoryServer._
    import MailHeaders._
    new InMemoryMessage(
      uid = uid,
      flags = Set.empty,
      headers = InMemory() +
        (Subject -> ("Test " + uid)) +
        (From -> Seq(Imap.MailboxAddress("foo" -> "bar"))) +
        (Date -> ZonedDateTime.now()) +
        (MessageId -> ("foo" + uid -> ("bar" + uid))) +
        (To -> Seq(Imap.MailboxAddress("baz" -> "qux"))),
      body = "Test message " + uid
    )
  }
    
  def createTestUser() = {
    import InMemoryServer._
    "foo" -> new InMemoryUser(
      username = "foo",
      password = "bar",
      mailboxes = Map(
        "INBOX" -> new InMemoryMailbox(
          "INBOX",
          flags = Set(Imap.Flag.Answered),
          permanentFlags = Set.empty,
          messages = for (uid <- 1 to 30) yield newMessage(uid)
        )
      )
    )
  }
}