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

class JavaMailSpec extends SpecificationWithJUnit with JavaMailMemoryServer {
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
    
    // TODO: more commands like LIST
//    "Should handle more scenarios" >> { ctx: Context =>
//      // We try to hit the commands we didn't in the previous test
//      ctx.server.users += createTestUser
//      ctx.daemon.start()
//      // Let's see the children of the default folder
//      val defaultFolder = ctx.store.getDefaultFolder
//      val folders = defaultFolder.list
//      folders.length === 27
//      // Open the inbox in read-write for fun...
//      val inbox = ctx.store.getFolder("INBOX")
//      inbox.open(Folder.READ_WRITE)
//      val msgs = inbox.getMessages
//      msgs.length === 30
//      1 === 1
//    }
    
    // TODO: Pending https://groups.google.com/forum/#!topic/akka-user/yPtCVRXPW10
//    "Should be able to handle TLS" >> { ctx: Context =>
//      ctx.useTls()
//      ctx.server.users += createTestUser
//      ctx.daemon.start()
//      val inbox = ctx.store.getFolder("INBOX")
//      inbox.open(Folder.READ_WRITE)
//      val msgs = inbox.getMessages
//      1 === 1
//    }
  }
    
  def createTestUser() = {
    import InMemoryServer._
    import MailHeaders._
    def newMessage(uid: Int) = new InMemoryMessage(
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