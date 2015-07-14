package scimap
package handler

import java.time.ZonedDateTime
import scimap.Imap.BodyStructureSingleExtension
import scala.concurrent.Future
import scala.util.matching.Regex
import java.util.regex.Pattern

// Basically only for testing
class InMemoryServer extends HighLevelServer {
  import HighLevelServer._
  import InMemoryServer._
  
  @volatile
  var users = Map.empty[String, InMemoryUser]
  @volatile
  var currentUser = Option.empty[InMemoryUser]
  @volatile
  var currentMailbox = Option.empty[InMemoryMailbox]
  @volatile
  var currentMailboxReadOnly = false
  @volatile
  var _capabilities = Seq(
    Imap.Capability.Imap4Rev1,
    Imap.Capability.StartTls,
    Imap.Capability.AuthPlain
  )
  @volatile
  var listenCallback = Option.empty[CallbackUpdate => Unit]
  
  override def listen(f: Option[CallbackUpdate => Unit]): Unit = listenCallback = f
  
  override def capabilities() = Future.successful(_capabilities)
  
  def authenticatePlain(username: String, password: String): Future[Boolean] =
    users.get(username) match {
      case Some(user) if user.password == password =>
        currentUser = Some(user)
        Future.successful(true)
      case _ =>
        Future.successful(false)
    }
  
  def select(mailbox: String, readOnly: Boolean): Future[Option[Mailbox]] =
    Future.successful(currentUser.flatMap(_.mailboxes.get(mailbox)).map { ret =>
      currentMailbox = Some(ret)
      currentMailboxReadOnly = readOnly
      ret
    })
  
  def list(tokenSets: Seq[Seq[Imap.ListToken]], startsAtRoot: Boolean): Future[Seq[ListItem]] = {
    // TODO: For now we just regex. Obviously this could be improved.
    // Obtain all names of all folders...
    def folderNames(prefix: String, folders: Iterable[InMemoryFolder]): Map[String, InMemoryFolder] = {
      folders.flatMap({ folder =>
        val name = prefix + hierarchyDelimiter.get + folder.name
        folderNames(name, folder._children) + (name -> folder)
      }).toMap
    }
    val allFolders =
      if (startsAtRoot || currentMailbox.isEmpty) folderNames("", currentUser.get.mailboxes.values)
      else folderNames("", Iterable(currentMailbox.get))
    // Go over each token set fetching
    println("FOLDER NAMES: " + allFolders.keys.toSeq)
    val foundFolders = tokenSets.flatMap({ tokenSet =>
      val regexString = tokenSet.foldLeft(if (startsAtRoot) "" else "/") { case (regex, token) =>
        regex + (token match {
          case Imap.ListToken.Str(str) => Pattern.quote(str)
          case Imap.ListToken.Delimiter => Pattern.quote(hierarchyDelimiter.get)
          // This next part only matches to next delimiter
          // Needs to handle if this is at the end or if the delim is > a single char
          case Imap.ListToken.NameWildcard => "[^" + Pattern.quote(hierarchyDelimiter.get) + "]*"
          case Imap.ListToken.PathWildcard => ".*"
        })
      }
      println("REGEX " + regexString + " FROM TOKENS: " + tokenSet)
      // Compile and get all folders that match it
      val pattern = Pattern.compile(regexString)
      allFolders.filterKeys(pattern.matcher(_).matches)
    }).toMap
    println("FOUND: " + foundFolders.keys.toSeq)
    // Convert to list items
    Future.successful(foundFolders.map({ case (name, folder) =>
      // Mailboxes are the only selectable ones
      // TODO: handle unmarked
      val attrs = 
        if (folder.isInstanceOf[InMemoryMailbox]) Seq.empty[Imap.ListAttribute]
        else Seq(Imap.ListAttribute.NoSelect)
      ListItem(name, attrs)
    }).toSeq)
  }
  
  def flushCurrentMailboxDeleted(): Future[Unit] = Future.successful(currentMailbox.foreach(_.flushDeleted()))
  
  def closeCurrentMailbox(): Future[Unit] = Future.successful(currentMailbox = None)
  
  def close(): Future[Unit] = Future.successful(())
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
    @volatile var _children: Seq[InMemoryFolder] = Seq.empty
  ) extends Folder {
    @volatile
    var listenCallback = Option.empty[CallbackUpdate => Unit]
    def listen(f: Option[CallbackUpdate => Unit]): Unit = listenCallback = f
    
    override def children(): Future[Seq[Folder]] = Future.successful(_children)
  }
  
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
    
    def getMessages(start: BigInt, end: BigInt): Future[Option[Seq[Message]]] = {
      val lifted = messages.lift
      val msgs = for (i <- start to end) yield {
        lifted(i.toInt - 1).getOrElse(return Future.successful(None))
      }
      Future.successful(Some(msgs))
    }
    
    def addMessage(msg: InMemoryMessage): Unit = {
      messages :+= msg
      println("LISTEN CALLBACK: " + listenCallback)
      listenCallback.foreach(_(CallbackUpdate.Exists(exists)))
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
    
    def markSeen(): Future[Unit] = Future.successful(flags += Imap.Flag.Seen)
    def getPart(part: Seq[Int]): Future[Option[String]] = ???
    def getHeader(part: Seq[Int], name: String): Future[Seq[String]] =
      if (!part.isEmpty) ???
      else Future.successful(MailHeaders.typeFromString(name).toSeq.flatMap(headers.lines(_)))
    def getHeaders(part: Seq[Int], notIncluding: Seq[String]): Future[Seq[String]] = {
      val not = notIncluding.flatMap(MailHeaders.typeFromString(_).toSeq)
      Future.successful(headers.headers.keys.filterNot(not.contains).flatMap(headers.lines(_)).toSeq)
    }
    def getMime(part: Seq[Int]): Future[Option[String]] = ???
    def getText(part: Seq[Int], offset: Option[Int], count: Option[Int]): Future[Option[String]] =
      if (!part.isEmpty) ???
      else if (offset.isEmpty) Future.successful(Some(body))
      else Future.successful(
        Some(body.substring(offset.get, Math.min(body.length, count.getOrElse(body.length) - offset.get)))
      )
  }
}