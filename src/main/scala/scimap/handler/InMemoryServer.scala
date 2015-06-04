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
  
  def authenticatePlain(username: String, password: String): Boolean =
    users.get(username) match {
      case Some(user) if user.password == password => currentUser = Some(user); true
      case _ => false
    }
  
  def examine(mailbox: String): Option[Mailbox] =
    currentUser.flatMap(_.mailboxes.get(mailbox)).map { ret =>
      currentMailbox = Some(ret)
      ret
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
  
  class InMemoryMailbox(
    @volatile var name: String,
    @volatile var messages: Seq[InMemoryMessage],
    @volatile var flags: Set[Imap.Flag],
    @volatile var permanentFlags: Set[Imap.Flag],
    @volatile var uidValidity: BigInt = System.currentTimeMillis
  ) extends Mailbox {
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
        date = headers(MailHeaders.OrigDate),
        subject = headers(MailHeaders.Subject),
        from = headers(MailHeaders.From).getOrElse(Seq.empty),
        sender = headers(MailHeaders.Sender).toSeq,
        replyTo = headers(MailHeaders.From).getOrElse(Seq.empty),
        to = headers(MailHeaders.From).getOrElse(Seq.empty),
        cc = headers(MailHeaders.From).getOrElse(Seq.empty),
        bcc = headers(MailHeaders.From).getOrElse(Seq.empty),
        inReplyTo = headers(MailHeaders.InReplyTo),
        messageId = headers(MailHeaders.MessageId)
      )
    }
    
    def internalDate: ZonedDateTime = headers(MailHeaders.OrigDate).getOrElse(sys.error("Can't find date"))
    // TODO: confirm whether this is just body or not
    def size: BigInt = body.length
    
    def getBody(part: Seq[Imap.BodyPart], offset: Option[Int], count: Option[Int]): Option[String] = {
      val result = peekBody(part, offset, count)
      flags += Imap.Flag.Seen
      result
    }
    
    def peekBody(part: Seq[Imap.BodyPart], offset: Option[Int], count: Option[Int]): Option[String] = {
      // TODO: provide a way to give errors here
      val possibleStr = part match {
        case Seq() => peekBody(Seq(Imap.BodyPart.Header), offset.map(_ => 0), count).flatMap(h =>
          peekBody(Seq(Imap.BodyPart.Text), offset.map(_ => 0), count).map(h + "\r\n\r\n" + _)
        )
        case Seq(Imap.BodyPart.Header) => Some(
          headers.headers().map(h => h._1 + ": " + h._2).mkString("\r\n")
        )
        case Seq(Imap.BodyPart.Text) => Some(body)
        case _ => None
      }
      val str = possibleStr.getOrElse(return None)
      offset -> count match {
        case (None, _) => Some(str)
        case (Some(offset), None) => None
        case (Some(offset), _) if offset > str.length => None
        case (Some(offset), Some(count)) if count > str.length => Some(str.substring(offset))
        case (Some(offset), Some(count)) => Some(str.substring(offset, count))
      }
    }
  }
}