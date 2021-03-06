package smail
package imap

import scala.collection.JavaConversions._
import org.specs2.mutable.SpecificationWithJUnit
import smail.imap.JavaMailMemoryServer.Context
import javax.mail.Folder
import smail.imap.handler.InMemoryServer
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
import smail.imap.handler.InMemoryServer.InMemoryMailbox
import javax.mail.internet.MimeMessage
import smail.imap.handler.InMemoryServer.InMemoryMailbox
import javax.mail.Flags.Flag
import javax.mail.search.FlagTerm
import smail.imap.handler.HighLevelServer
import java.time.OffsetDateTime

class JavaMailSpec extends SpecificationWithJUnit with JavaMailMemoryServer {
  sequential
  
  type EE = ExecutionEnv
  
  "JavaMail API" >> {

    "Should be able to fetch simple messages" >> { ctx: Context =>
      ctx.server.state.users += createTestUser
      ctx.startDaemon()
      val inbox = ctx.store.getFolder("INBOX")
      inbox.open(Folder.READ_ONLY)
      val msgs = inbox.getMessages
      msgs.length === 30
      val secondMsg = msgs(1)
      // At first it's unseen
      secondMsg.getFlags === new Flags(Flags.Flag.RECENT)
      secondMsg.getAllRecipients.toSeq === Seq(new InternetAddress("baz@qux"))
      secondMsg.getContent === "Test message 2"
      // Make sure it's now considered "seen"
      secondMsg.asInstanceOf[IMAPMessage].invalidateHeaders()
      secondMsg.getFlags === new Flags(Flags.Flag.SEEN)
      secondMsg.getFrom.toSeq === Seq(new InternetAddress("foo@bar"))
      secondMsg.getMessageNumber === 2
      // No ms in imap
      val msg = ctx.server.currentMailbox.get.messages(1)
      val date = msg.headerList[Message.Field.OrigDate]
      secondMsg.getReceivedDate.getTime === date.head.value.toEpochSecond * 1000
      // For now I reuse this
      secondMsg.getSentDate === secondMsg.getReceivedDate
      secondMsg.getSubject === "Test 2"
      val actualHeaders = secondMsg.getAllHeaders.map(_.asInstanceOf[Header]).map(h => h.getName -> h.getValue).toSet
      val expectedHeaders = msg.headers.map(f => f.prefix -> f.valueToString()).toSet
      actualHeaders === expectedHeaders
      secondMsg.getContentType === "TEXT/PLAIN; charset=US-ASCII"
      Option(secondMsg.getDescription) === None
      Option(secondMsg.getDisposition) === None
      Option(secondMsg.getFileName) === None
      secondMsg.getHeader("Message-ID").toSeq === Seq("<foo2@bar2>")
      secondMsg.getLineCount === 1
      secondMsg.getSize === 14
    }
    
    "Should list properly" >> { ctx: Context =>
      // We try to hit the commands we didn't in the previous test
      val (testUsername, testUser) = createTestUser
      ctx.server.state.users += testUsername -> testUser
      ctx.startDaemon()
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
      ctx.server.state.users += testUsername -> testUser
      ctx.startDaemon()
      val inbox = ctx.store.getFolder("INBOX")
      inbox.open(Folder.READ_WRITE)
      inbox.getMessages.length === 30
      @volatile var countEvents = Seq.empty[MessageCountEvent]
      inbox.addMessageCountListener(new MessageCountListener {
        override def messagesAdded(e: MessageCountEvent) = countEvents :+= e
        override def messagesRemoved(e: MessageCountEvent) = countEvents :+= e
      })
      // Add a message
      addNewMessage(testUser.folders.values.head.asInstanceOf[InMemoryMailbox], 3000)
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
    
    "Should handle idle count update" >> { implicit ee: EE => ctx: Context =>
      val (testUsername, testUser) = createTestUser
      ctx.server.state.users += testUsername -> testUser
      ctx.server._capabilities = Some(HighLevelServer.defaultCapabilities :+ Imap.Capability.Idle)
      ctx.startDaemon()
      val inbox = ctx.store.getFolder("INBOX")
      inbox.open(Folder.READ_WRITE)
      inbox.getMessages.length === 30
      @volatile var countEvents = Seq.empty[MessageCountEvent]
      inbox.addMessageCountListener(new MessageCountListener {
        override def messagesAdded(e: MessageCountEvent) = countEvents :+= e
        override def messagesRemoved(e: MessageCountEvent) = countEvents :+= e
      })
      // Put into idle to get updates
      Future { inbox.asInstanceOf[IMAPFolder].idle() }
      // Wait just a bit
      Thread.sleep(200)
      // Add a message
      addNewMessage(testUser.folders.values.head.asInstanceOf[InMemoryMailbox], 3000)
      countEvents.size must be_==(1).eventually
      countEvents(0).getType === MessageCountEvent.ADDED
      countEvents(0).getMessages.length === 1
      val msg = countEvents(0).getMessages()(0)
      msg.getMessageNumber === 31
      inbox.asInstanceOf[UIDFolder].getUID(msg) === 3000
      msg.getSubject === "Test 3000"
      msg.getContent === "Test message 3000"
    }

    "Should be able to handle TLS" >> { ctx: Context =>
      ctx.useTls()
      ctx.server.state.users += createTestUser
      ctx.startDaemon()
      val inbox = ctx.store.getFolder("INBOX")
      inbox.open(Folder.READ_WRITE)
      val msgs = inbox.getMessages
      msgs.length === 30
      val secondMsg = msgs(1)
      secondMsg.getFlags === new Flags(Flags.Flag.RECENT)
      secondMsg.getAllRecipients.toSeq === Seq(new InternetAddress("baz@qux"))
    }

    "Should be able to create a new mailbox and rename it and delete it" >> { ctx: Context =>
      ctx.server.state.users += createTestUser
      ctx.startDaemon()
      val topFolder = ctx.store.getFolder("NewTestFolder")
      topFolder.exists() === false
      topFolder.getFolder("NewTestMailbox").create(Folder.HOLDS_MESSAGES) === true
      topFolder.exists() === true
      // Grab all top level folders and make sure the two we expect are there
      val folders = ctx.store.getDefaultFolder.list()
      folders.length === 2
      folders.exists(_.getName == "INBOX") === true
      println("REAL NAME: " + folders.map(_.getFullName).toSeq)
      folders.exists(_.getFullName == "INBOX") === true
      folders.exists(_.getName == "NewTestFolder") === true
      folders.exists(_.getFullName == "NewTestFolder") === true
      val childFolders = folders.find(_.getName == "NewTestFolder").get.list()
      childFolders.length === 1
      childFolders(0).getName === "NewTestMailbox"
      childFolders(0).getFullName === "NewTestFolder/NewTestMailbox"
      // Now rename
      childFolders(0).renameTo(topFolder.getFolder("OldTestMailbox")) === true
      val updatedFolders = topFolder.list()
      updatedFolders.length === 1
      updatedFolders(0).getName === "OldTestMailbox"
      updatedFolders(0).getFullName === "NewTestFolder/OldTestMailbox"
      // Now recursively delete from the top
      topFolder.delete(true)
      topFolder.exists() === false
    }

    "Should be able to subscribe, unsubscribe, and list subscribed" >> { ctx: Context =>
      ctx.server.state.users += createTestUser
      ctx.startDaemon()
      ctx.store.getFolder("/NewTestSubMailbox").create(Folder.HOLDS_MESSAGES) === true
      ctx.store.getFolder("/NewTestSubMailbox").isSubscribed() === false
      ctx.store.getFolder("/NewTestSubMailbox").setSubscribed(true)
      ctx.store.getFolder("/NewTestSubMailbox").isSubscribed() === true
      ctx.store.getFolder("/NewTestSubMailbox").setSubscribed(false)
      ctx.store.getFolder("/NewTestSubMailbox").isSubscribed() === false
    }

    "Should be able to fetch a status value" >> { ctx: Context =>
      ctx.server.state.users += createTestUser
      ctx.startDaemon()
      ctx.store.getFolder("/INBOX").asInstanceOf[IMAPFolder].getStatusItem("MESSAGES") === 30
    }

    "Should be able to append messages" >> { ctx: Context =>
      ctx.server.state.users += createTestUser
      ctx.startDaemon()
      val folder = ctx.store.getFolder("/INBOX")
      folder.getMessageCount === 30
      // Add one w/ a subject
      val msg = new MimeMessage(ctx.session)
      msg.setSubject("Append Test")
      msg.setText("Append Test Body\r\nAnd more info")
      folder.appendMessages(Array(msg))
      val updatedFolder = ctx.store.getFolder("/INBOX")
      updatedFolder.getMessageCount === 31
      updatedFolder.open(Folder.READ_ONLY)
      val last = updatedFolder.getMessages.last
      last.getSubject === "Append Test"
      last.getContent === "Append Test Body\r\nAnd more info"
    }

    "Should expunge deleted" >> { ctx: Context =>
      val user = createTestUser
      ctx.server.state.users += user
      ctx.startDaemon()
      // Let's mark messages 5 and 10 as deleted
      val mailbox = user._2.folders.head._2.asInstanceOf[InMemoryMailbox]
      mailbox.messages.find(_.uid == 5).get.flags = Set(Imap.Flag.Deleted)
      mailbox.messages.find(_.uid == 12).get.flags = Set(Imap.Flag.Deleted)
      // Confirm there are still 30
      val folder = ctx.store.getFolder("/INBOX")
      folder.getMessageCount === 30
      // Expunge the two
      folder.open(Folder.READ_WRITE)
      val msgs = folder.expunge()
      // Confirm expunged
      ctx.store.getFolder("/INBOX").getMessageCount === 28
      msgs.map(_.getMessageNumber).toSet === Set(5, 12)
    }
    
    "Should search" >> { ctx: Context =>
      ctx.server.state.users += createTestUser
      ctx.startDaemon()
      // Let's take every message with subject containing 5 or body containing 7
      val folder = ctx.store.getFolder("/INBOX")
      folder.open(Folder.READ_ONLY)
      import javax.mail.search._
      val msgs = folder.search(new OrTerm(new SubjectTerm("5"), new BodyTerm("7")))
      msgs.map(_.getMessageNumber).toSet === Set(5, 15, 25, 7, 17, 27)
    }
    
    "Should store" >> { ctx: Context =>
      val user = createTestUser
      ctx.server.state.users += user
      ctx.startDaemon()
      // Add Seen to 15 through 20
      val folder = ctx.store.getFolder("/INBOX")
      folder.open(Folder.READ_WRITE)
      folder.setFlags(15, 20, new Flags(Flags.Flag.SEEN), true)
      // Ask for all seen
      val msgs = folder.search(new FlagTerm(new Flags(Flags.Flag.SEEN), true))
      msgs.map(_.getMessageNumber).toSet === (15 to 20).toSet
      // Remove seen from 12 through 16 (which means really only 15 and 16)
      folder.setFlags(12, 16, new Flags(Flags.Flag.SEEN), false)
      val updatedMsgs = folder.search(new FlagTerm(new Flags(Flags.Flag.SEEN), true))
      updatedMsgs.map(_.getMessageNumber).toSet === (17 to 20).toSet
      // No way to test replace w/ java mail :-(
    }

    "Should copy" >> { ctx: Context =>
      ctx.server.state.users += createTestUser
      ctx.startDaemon()
      ctx.store.getFolder("/NewTestSubMailbox").create(Folder.HOLDS_MESSAGES) === true
      // Make sure it has no messages at first
      ctx.store.getFolder("/NewTestSubMailbox").getMessageCount === 0
      // Copy over messages 7 to 11 from inbox
      val inbox = ctx.store.getFolder("/INBOX")
      inbox.open(Folder.READ_ONLY)
      inbox.copyMessages(inbox.getMessages(7, 11), ctx.store.getFolder("/NewTestSubMailbox"))
      // Now make sure it has the right messages
      val newFolder = ctx.store.getFolder("/NewTestSubMailbox")
      newFolder.open(Folder.READ_ONLY)
      newFolder.getMessageCount === 5
      newFolder.getMessages.map(_.getSubject).toSet === (7 to 11).map("Test " + _).toSet
    }
  }
  
  def addNewMessage(mailbox: InMemoryMailbox, uid: Int) = {
    import InMemoryServer._
    import Message.Address._
    import Message.Field._
    mailbox.addMessage(new InMemoryMessage(
      mailbox = mailbox,
      uid = uid,
      flags = Set.empty,
//      headers = InMemory() +
//        (Subject -> ("Test " + uid)) +
//        (From -> Seq(Imap.MailboxAddress("foo" -> "bar"))) +
//        (Date -> ZonedDateTime.now()) +
//        (MessageId -> ("foo" + uid -> ("bar" + uid))) +
//        (To -> Seq(Imap.MailboxAddress("baz" -> "qux"))),
      headers = Seq(
        Subject("Test " + uid),
        From(Seq(Mailbox("foo" -> "bar"))),
        OrigDate(OffsetDateTime.now()),
        MessageId(Message.MsgId("foo" + uid, "bar" + uid)),
        To(Seq(Mailbox("baz" -> "qux")))
      ),
      body = "Test message " + uid
    ))
  }
    
  def createTestUser() = {
    import InMemoryServer._
    val mailbox = new InMemoryMailbox(
      "INBOX",
      flags = Set(Imap.Flag.Answered),
      permanentFlags = Set.empty
    )
    for (uid <- 1 to 30) addNewMessage(mailbox, uid)
    "foo" -> new InMemoryUser(
      username = "foo",
      password = "bar",
      folders = Map(
        "INBOX" -> mailbox
      )
    )
  }
}