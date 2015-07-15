package scimap
package handler

import java.time.ZonedDateTime
import scimap.Imap.BodyStructureSingleExtension
import scala.concurrent.Future
import scala.util.matching.Regex
import java.util.regex.Pattern
import scala.util.Try

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
  
  def getFolder(name: String): Option[InMemoryFolder] = {
    val pieces = name.stripPrefix(hierarchyDelimiter.get).
      stripSuffix(hierarchyDelimiter.get).split(hierarchyDelimiter.get)
    pieces.tail.foldLeft(currentUser.get.folders.get(pieces.head)) {
      case (None, _) => None
      case (Some(parent), childName) => parent._children.find(_.name == childName)
    }
  }
  
  def get(mailbox: String): Future[Option[Mailbox]] =
    Future.successful(getFolder(mailbox).filter(_.isInstanceOf[Mailbox]).map(_.asInstanceOf[Mailbox]))
  
  def select(mailbox: String, readOnly: Boolean): Future[Option[Mailbox]] =
    Future.successful(get(mailbox).value.get.get.map { mailbox =>
      currentMailbox = Some(mailbox.asInstanceOf[InMemoryMailbox])
      currentMailboxReadOnly = readOnly
      mailbox
    })
    
  def create(name: String): Future[Option[String]] = {
    // If it ends with the separator, it's just a folder, otherwise it's a mailbox
    val isFolder = name.endsWith(hierarchyDelimiter.get)
    Future.successful(
      create(name, if (isFolder) new InMemoryFolder("") else new InMemoryMailbox(""))
    )
  }
  
  def create(name: String, item: InMemoryFolder): Option[String] = {
    // If it ends with the separator, it's just a folder, otherwise it's a mailbox
    val isFolder = name.endsWith(hierarchyDelimiter.get)
    // Trim it and split it across the delimiter
    val pieces = name.stripSuffix(hierarchyDelimiter.get).
      stripPrefix(hierarchyDelimiter.get).split(hierarchyDelimiter.get)
    // Set the name on the item
    item.name = pieces.last
    // If there is only one item, it is top level
    if (pieces.length == 1) {
      currentUser.get.folders += item.name -> item
      return None
    }
    // Create all needed parents to satisfy this
    val parent = pieces.dropRight(1).foldLeft(Option.empty[InMemoryFolder])({ case (parent, name) =>
      Some(parent.flatMap(_._children.find(_.name == name)).getOrElse {
        val child = new InMemoryFolder(name)
        parent match {
          case Some(parent) =>
            parent._children :+= child
            child
          case None =>
            currentUser.get.folders += name -> child
            child
        }
      })
    }).get
    // Failure if the child already exists
    if (parent._children.exists(_.name == pieces.last)) Some("Cannot find parent mailbox")
    else {
      parent._children :+= item
      None
    }
  }
  
  def delete(name: String): Future[Option[String]] =
    Future.successful(deleteFolder(name).right.toOption)
  
  def deleteFolder(name: String): Either[InMemoryFolder, String] = {
    // First find the parents
    val namePieces = name.stripPrefix(hierarchyDelimiter.get).
      stripSuffix(hierarchyDelimiter.get).split(hierarchyDelimiter.get)
    if (namePieces.length == 1) {
      currentUser.get.folders.get(namePieces.head) match {
        case Some(child) =>
          currentUser.get.folders -= namePieces.head
          Left(child)
        case None =>
          Right("Mailbox not found")
      }
    } else {
      val parents = namePieces.dropRight(1).foldLeft(Seq.empty[InMemoryFolder]) {
        case (Seq(), name) => currentUser.get.folders.get(name).toSeq
        case (parents, childName) => parents.flatMap(_._children.find(_.name == childName))
      }
      val folder = parents.flatMap({ parent =>
        parent._children.find(_.name == namePieces.last).map(parent -> _)
      }).headOption
      folder.map({ case (parent, child) =>
        // It can't have any mailbox children
        if (child.hasMailboxChild()) Right("Mailbox has child mailboxes")
        else {
          parent._children = parent._children.filterNot(_.name == child.name)
          Left(child)
        }
      }).getOrElse(Right("Mailbox not found"))
    }
  }
    
  def rename(oldName: String, newName: String): Future[Option[String]] = {
    val err = deleteFolder(oldName) match {
      case Right(err) => Some(err)
      case Left(folder) => create(newName.stripPrefix(hierarchyDelimiter.get), folder)
    }
    Future.successful(err)
  }
  
  def subscribe(name: String): Future[Boolean] = {
    // We know about the race conditions here but we don't care in a test server
    val prevSize = currentUser.get.subscribed.size
    currentUser.get.subscribed += name
    Future.successful(prevSize != currentUser.get.subscribed.size)
  }
  
  def unsubscribe(name: String): Future[Boolean] = {
    // We know about the race conditions here but we don't care in a test server
    val prevSize = currentUser.get.subscribed.size
    currentUser.get.subscribed -= name
    Future.successful(prevSize != currentUser.get.subscribed.size)
  }
  
  def allFolderNames(
    prefix: String = "",
    folders: Iterable[InMemoryFolder] = currentUser.get.folders.values
  ): Map[String, InMemoryFolder] = {
    folders.flatMap({ folder =>
      val name = prefix + hierarchyDelimiter.get + folder.name
      allFolderNames(name, folder._children) + (name -> folder)
    }).toMap
  }
  
  def list(tokens: Seq[Imap.ListToken], startsAtRoot: Boolean, subscribedOnly: Boolean): Future[Seq[ListItem]] = {
    // TODO: For now we just regex. Obviously this could be improved.
    val allFolders =
      // TODO: LSUB technically can return values that are deleted...but we don't allow that here
      if (subscribedOnly) {
        allFolderNames("", currentUser.get.folders.values).filterKeys(currentUser.get.subscribed.contains)
      } else if (startsAtRoot || currentMailbox.isEmpty) allFolderNames("", currentUser.get.folders.values)
      else allFolderNames("", Iterable(currentMailbox.get))
    
    // Go over each token set fetching
    val regexString = tokens.foldLeft(Pattern.quote(hierarchyDelimiter.get)) { case (regex, token) =>
      regex + (token match {
        case Imap.ListToken.Str(str) => Pattern.quote(str)
        case Imap.ListToken.Delimiter => Pattern.quote(hierarchyDelimiter.get)
        // This next part only matches to next delimiter
        // Needs to handle if this is at the end or if the delim is > a single char
        case Imap.ListToken.NameWildcard => "[^" + Pattern.quote(hierarchyDelimiter.get) + "]*"
        case Imap.ListToken.PathWildcard => ".*"
      })
    }
    // Compile and get all folders that match it
    val pattern = Pattern.compile(regexString)
    val foundFolders = allFolders.filterKeys(pattern.matcher(_).matches)
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
    @volatile var folders: Map[String, InMemoryFolder]
  ) {
    @volatile
    var subscribed = Set.empty[String]
  }
  
  class InMemoryFolder(
    @volatile var name: String,
    @volatile var _children: Seq[InMemoryFolder] = Seq.empty
  ) extends Folder {
    @volatile
    var listenCallback = Option.empty[CallbackUpdate => Unit]
    def listen(f: Option[CallbackUpdate => Unit]): Unit = listenCallback = f
    
    override def children(): Future[Seq[Folder]] = Future.successful(_children)
    
    def hasMailboxChild(): Boolean = _children.exists { child =>
      child.isInstanceOf[InMemoryMailbox] || child.hasMailboxChild()
    }
  }
  
  class InMemoryMailbox(
    private var nameParam: String,
    @volatile var messages: Seq[InMemoryMessage] = Seq.empty,
    @volatile var flags: Set[Imap.Flag] = Set.empty,
    @volatile var permanentFlags: Set[Imap.Flag] = Set.empty,
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
    
    def addMessage(message: String, flags: Set[Imap.Flag], date: ZonedDateTime): Future[Option[String]] = {
      // Too keep this test server simple we're just gonna get all headers until first blank line...
      //  no multipart support for now
      val headersAndBody = message.split("\r\n\r\n", 2)
      if (headersAndBody.length != 2) return Future.successful(Some("Can't find headers and body"))
      // Parse headers (only the ones we can for now)
      val headers: Seq[(MailHeaders.Type[Any], Any)] = (headersAndBody(0).split("\r\n").flatMap { header =>
        val colonIndex = header.indexOf(":")
        if (colonIndex == -1) None
        else MailHeaders.typeFromString(header.substring(0, colonIndex)).flatMap { typ =>
          val t = Try(typ.valueFromString(header.substring(colonIndex + 1)))
          Try(typ.valueFromString(header.substring(colonIndex + 1).trim)).toOption.map { v =>
            typ.asInstanceOf[MailHeaders.Type[Any]] -> v
          }
        }
      })
      // Add message w/ body, flags, and overridden date
      addMessage(
        new InMemoryMessage(
          uid = nextUid,
          flags = flags,
          headers = headers.foldLeft(MailHeaders.InMemory())(_ + _) + (MailHeaders.Date -> date),
          body = headersAndBody(1)
        )
      )
      Future.successful(None)
    }
    
    def addMessage(msg: InMemoryMessage): Unit = {
      messages :+= msg
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