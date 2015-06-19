package scimap
package handler

import java.time.ZonedDateTime
import scimap.Imap.BodyStructureSingleExtension

// Basically only for testing
class InMemoryServer extends HighLevelServer {
  import HighLevelServer._
  import InMemoryServer._
  
  @volatile
  var users = Map.empty[String, InMemoryUser]
  var currentUser = Option.empty[InMemoryUser]
  var currentMailbox = Option.empty[InMemoryMailbox]
  var currentMailboxReadOnly = false
  
  def authenticatePlain(username: String, password: String): Boolean =
    users.get(username) match {
      case Some(user) if user.password == password => currentUser = Some(user); true
      case _ => false
    }
  
  def select(mailbox: String, readOnly: Boolean): Option[Mailbox] =
    currentUser.flatMap(_.mailboxes.get(mailbox)).map { ret =>
      currentMailbox = Some(ret)
      currentMailboxReadOnly = readOnly
      ret
    }
  
  def list(tokenSets: Seq[Seq[Imap.ListToken]], startsAtRoot: Boolean): Seq[ListItem] = {
    // TODO: do we start at the current mailbox?
//    println("Asking for", tokens)
//    var currentMailboxes =
//      if (startsAtRoot) Map.empty[String, Mailbox]
//      else Map("/")
//    // Go over each token, updating the list of mailboxes
//    tokens.zipWithIndex.foreach { case (token, index) =>
//      token match
//    }
    ???
  }
  
  def flushCurrentMailboxDeleted(): Unit = currentMailbox.foreach(_.flushDeleted())
  
  def closeCurrentMailbox(): Unit = currentMailbox = None
  
  def close(): Unit = { }
}
object InMemoryServer {
  import HighLevelServer._
  
  class InMemoryUser(
    @volatile var username: String,
    @volatile var password: String,
    @volatile var mailboxes: Map[String, InMemoryMailbox]
  )
  
  class InMemoryFolder(
    @volatile var name: String,
    @volatile var children: Seq[InMemoryFolder] = Seq.empty
  ) extends Folder
  
  class InMemoryMailbox(
    private var nameParam: String,
    @volatile var messages: Seq[InMemoryMessage],
    @volatile var flags: Set[Imap.Flag],
    @volatile var permanentFlags: Set[Imap.Flag],
    @volatile var uidValidity: BigInt = System.currentTimeMillis,
    private var childrenParam: Seq[InMemoryFolder] = Seq.empty
  ) extends InMemoryFolder(nameParam, childrenParam) with Mailbox {
    def exists = messages.size
    def recent = messages.count(_.flags.contains(Imap.Flag.Recent))
    def firstUnseen = messages.find(!_.flags.contains(Imap.Flag.Seen)).map(_.uid).getOrElse(0)
    def nextUid = messages.foldLeft(BigInt(0)){ case (max, msg) => max.max(msg.uid) } + 1
    
    def getMessages(start: BigInt, end: BigInt): Option[Seq[Message]] = {
      println("Getting from ", start, end)
      val lifted = messages.lift
      val msgs = for (i <- start to end) yield {
        lifted(i.toInt - 1).getOrElse(return None)
      }
      Some(msgs)
    }
    
    def flushDeleted(): Unit = messages = messages.filterNot(_.flags.contains(Imap.Flag.Deleted))
  }
  
  class InMemoryMessage(
    @volatile var uid: BigInt,
    @volatile var flags: Set[Imap.Flag],
    @volatile var headers: MailHeaders,
    @volatile var body: String
  ) extends Message {
    def bodyStructure: Imap.BodyStructure = {
      // Just single part for now...
      Imap.BodyStructureSingle(
        bodyType = "TEXT",
        subType = "PLAIN",
        parameters = Map("CHARSET" -> "US-ASCII"),
        id = None,
        description = None,
        encoding = Some("7BIT"),
        size = body.length,
        lineCount = Some("\r\n".r.findAllMatchIn(body).length + 1),
        extension = Some(
          BodyStructureSingleExtension(
            // TODO: check encoding
            md5 = Some(Util.base64Encode(Util.md5(body.getBytes)))
            // TODO: other info?
          )
        )
      )
    }
    
    def envelope: Imap.Envelope = {
      Imap.Envelope(
        date = headers(MailHeaders.Date),
        subject = headers(MailHeaders.Subject),
        from = headers(MailHeaders.From).getOrElse(Seq.empty),
        sender = headers(MailHeaders.Sender).toSeq,
        replyTo = headers(MailHeaders.From).getOrElse(Seq.empty),
        to = headers(MailHeaders.To).getOrElse(Seq.empty),
        cc = headers(MailHeaders.Cc).getOrElse(Seq.empty),
        bcc = headers(MailHeaders.Bcc).getOrElse(Seq.empty),
        inReplyTo = headers(MailHeaders.InReplyTo).getOrElse(Seq.empty),
        messageId = headers(MailHeaders.MessageId)
      )
    }
    
    def internalDate: ZonedDateTime = headers(MailHeaders.Date).getOrElse(sys.error("Can't find date"))
    // TODO: confirm whether this is just body or not
    def size: BigInt = body.length
    
    def markSeen(): Unit = flags += Imap.Flag.Seen
    def getPart(part: Seq[Int]): Option[String] = ???
    def getHeader(part: Seq[Int], name: String): Seq[String] =
      if (!part.isEmpty) ???
      else MailHeaders.typeFromString(name).toSeq.flatMap(headers.lines(_))
    def getHeaders(part: Seq[Int], notIncluding: Seq[String]): Seq[String] = {
      val not = notIncluding.flatMap(MailHeaders.typeFromString(_).toSeq)
      headers.headers.keys.filterNot(not.contains).flatMap(headers.lines(_)).toSeq
    }
    def getMime(part: Seq[Int]): Option[String] = ???
    def getText(part: Seq[Int], offset: Option[Int], count: Option[Int]): Option[String] =
      if (!part.isEmpty) ???
      else if (offset.isEmpty) Some(body)
      else Some(body.substring(offset.get, Math.min(body.length, count.getOrElse(body.length) - offset.get)))
  }
}