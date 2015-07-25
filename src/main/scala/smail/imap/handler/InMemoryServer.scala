package smail.imap
package handler

import java.time.ZonedDateTime
import scala.concurrent.Future
import java.util.regex.Pattern
import smail.Util
import scala.util.Try

// Basically only for testing
class InMemoryServer(val state: InMemoryServer.State) extends HighLevelServer {
  import HighLevelServer._
  import InMemoryServer._
  
  @volatile
  var currentUser = Option.empty[InMemoryUser]
  @volatile
  var currentMailbox = Option.empty[InMemoryMailbox]
  @volatile
  var currentMailboxReadOnly = false
  @volatile
  var currentMailboxLastEventIndex = 0
  @volatile
  var _capabilities = Option.empty[Seq[Imap.Capability]]
  @volatile
  var listenCallback = Option.empty[CallbackUpdate => Unit]
  
  override def getCurrentMailboxEvents(): Future[Map[BigInt, MailboxEvent]] = {
    currentMailbox match {
      case None => Future.successful(Map.empty)
      case Some(mailbox) =>
        val results = mailbox.events.drop(currentMailboxLastEventIndex).toMap
        currentMailboxLastEventIndex = mailbox.events.size - 1
        Future.successful(results)
    }
  }
  
  override def listen(f: Option[CallbackUpdate => Unit]): Unit = listenCallback = f
  
  override def capabilities() = _capabilities.map(Future.successful).getOrElse(super.capabilities())
  
  def authenticatePlain(username: String, password: String): Future[Boolean] =
    state.users.get(username) match {
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
      currentMailboxLastEventIndex = 0
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
  
  def copyMessagesFromCurrent(
    start: BigInt,
    end: Option[BigInt],
    toMailbox: String,
    byUid: Boolean
  ): Future[Option[String]] = {
    get(toMailbox).value.get.get match {
      case Some(newMailbox: InMemoryMailbox) =>
        currentMailbox.get.getInMemoryMessages(start, end, byUid).map(_._2).foreach(newMailbox.addMessage)
        Future.successful(None)
      case _ =>
        Future.successful(Some("Unrecognized mailbox: " + toMailbox))
    }
  }
  
  def closeCurrentMailbox(): Future[Unit] = {
    if (!currentMailboxReadOnly) currentMailbox.get.expunge()
    currentMailboxLastEventIndex = 0
    Future.successful(currentMailbox = None)
  }
  
  def close(): Future[Unit] = Future.successful(())
}
object InMemoryServer {
  import HighLevelServer._
  
  class State(
    @volatile var users: Map[String, InMemoryUser] = Map.empty
  )
  
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
    
    def children(): Future[Seq[Folder]] = Future.successful(_children)
    
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
    
    @volatile
    var events = Seq.empty[(BigInt, MailboxEvent)]
    
    def exists = messages.size
    def recent = messages.count(_.flags.contains(Imap.Flag.Recent))
    def firstUnseen = messages.find(!_.flags.contains(Imap.Flag.Seen)).map(_.uid).getOrElse(0)
    def unseen = messages.count(!_.flags.contains(Imap.Flag.Seen))
    def nextUid = messages.foldLeft(BigInt(0)){ case (max, msg) => max.max(msg.uid) } + 1
    
    def msgSeqFromUid(uid: BigInt): BigInt = messages.indexWhere(_.uid == uid) match {
      case -1 => sys.error("Not found")
      case index => index + 1
    }
    
    def getInMemoryMessages(start: BigInt, end: Option[BigInt], byUid: Boolean): Seq[(BigInt, InMemoryMessage)] = {
      var properStart = start
      var properEnd = end
      if (byUid) {
        // Don't have to be contiguous here...
        properStart = messages.indexWhere(_.uid == start) + 1
        if (properStart == 0)  {
          // Per the spec, non-end always returns the last message
          if (properEnd.isEmpty) properStart = messages.size
          else return Seq.empty
        } else properEnd.foreach { end =>
          properEnd = Some(messages.indexWhere(_.uid == end) + 1)
          // Has to be present and >= start
          if (properEnd.get == 0 || properEnd.get < start) return Seq.empty
        }
      }
      val lifted = messages.lift
      (properStart to properEnd.getOrElse(messages.size)).flatMap(i => lifted(i.toInt - 1).map(i -> _))
    }
    
    def getMessages(start: BigInt, end: Option[BigInt], byUid: Boolean): Future[Seq[(BigInt, Message)]] = {
      Future.successful(getInMemoryMessages(start, end, byUid))
    }
    
    def addMessage(message: String, flags: Set[Imap.Flag], date: ZonedDateTime): Future[Option[String]] = {
      // Too keep this test server simple we're just gonna get all headers until first blank line...
      //  no multipart support for now
      val headersAndBody = message.split("\r\n\r\n", 2)
      if (headersAndBody.length != 2) return Future.successful(Some("Can't find headers and body"))
      // Parse headers (only the ones we can for now)
      val headers: Seq[(MailHeaders.Type[Any], Any)] = (headersAndBody(0).split("\r\n").flatMap { header =>
        println("PARSING HEADER: " + header)
        val colonIndex = header.indexOf(":")
        if (colonIndex == -1) None
        else MailHeaders.typeFromString(header.substring(0, colonIndex)).flatMap { typ =>
          println("Result of " + header + ": " + Try(typ.valueFromString(header.substring(colonIndex + 1).trim)))
          Try(typ.valueFromString(header.substring(colonIndex + 1).trim)).toOption.map { v =>
            typ.asInstanceOf[MailHeaders.Type[Any]] -> v
          }
        }
      })
      // Add message w/ body, flags, and overridden date
      addMessage(
        new InMemoryMessage(
          mailbox = this,
          uid = nextUid,
          flags = flags,
          headers = headers.foldLeft(MailHeaders.InMemory())(_ + _) + (MailHeaders.Date -> date),
          body = headersAndBody(1)
        )
      )
      Future.successful(None)
    }
    
    def addMessage(msg: InMemoryMessage): Unit = {
      msg.flags += Imap.Flag.Recent
      messages :+= msg
      listenCallback.foreach(_(CallbackUpdate.Exists(exists)))
    }
    
    def checkpoint(): Future[Unit] = {
      // Noop
      Future.successful(())
    }
    
    def expunge(): Future[Seq[BigInt]] = {
      // Due to imap rules, we have to do one at a time so the sequence numbers match
      val seq = messages.foldLeft(Seq.empty[BigInt]) {
        case (seq, msg) if !msg.flags.contains(Imap.Flag.Deleted) => seq 
        case (seq, msg) =>
          val index = msgSeqFromUid(msg.uid)
          messages = messages.filterNot(_.uid == msg.uid)
          seq :+ index
      }
      seq.foreach(i => events :+= i -> MailboxEvent.MessageExpunged(i))
      Future.successful(seq)
    }
    
    def search(criteria: Seq[Imap.SearchCriterion], returnUids: Boolean): Future[Seq[BigInt]] = {
      val filters = criteria.map(criterionToFilter)
      val msgsWithIndex = messages.zipWithIndex.filterNot(idxAndMsg => filters.exists(!_(idxAndMsg._1)))
      Future.successful(
        msgsWithIndex.map { case (msg, index) =>
          if (returnUids) msg.uid else BigInt(index + 1)
        }
      )
    }
    
    def criterionToFilter(criterion: Imap.SearchCriterion): (InMemoryMessage) => Boolean = {
      import Imap.SearchCriterion._
      criterion match {
        case SequenceSet(set) => msg => set.contains(msg.uid)
        case All => _ => true
        case Answered => _.flags.contains(Imap.Flag.Answered)
        case Bcc(bcc) =>
          _.headers(MailHeaders.Bcc).map(_.exists(_.toString.contains(bcc))).getOrElse(false)
        case Before(date) => _.internalDate.toLocalDate.isBefore(date)
        case Body(str) => _.body.contains(str)
        case Cc(cc) =>
          _.headers(MailHeaders.Cc).map(_.exists(_.toString.contains(cc))).getOrElse(false)
        case Deleted => _.flags.contains(Imap.Flag.Deleted)
        case Draft => _.flags.contains(Imap.Flag.Draft)
        case Flagged => _.flags.contains(Imap.Flag.Flagged)
        case From(from) =>
          _.headers(MailHeaders.From).map(_.exists(_.toString.contains(from))).getOrElse(false)
        case Header(name, v) =>
          msg => MailHeaders.typeFromString(name).flatMap(typ =>
            msg.headers(typ).map(typ.asInstanceOf[MailHeaders.Type[Any]].valueToString).
              map(_.exists(_.contains(v)))
          ).getOrElse(false)
        case Keyword(flag) => _.flags.contains(flag)
        case Larger(size) => _.size > size
        case New => msg => msg.flags.contains(Imap.Flag.Recent) && !msg.flags.contains(Imap.Flag.Seen)
        case Not(crit) =>
          val filter = criterionToFilter(crit)
          !filter(_)
        case Old => !_.flags.contains(Imap.Flag.Recent)
        case On(date) => _.internalDate.toLocalDate.equals(date)
        case Or(crit1, crit2) =>
          val filter1 = criterionToFilter(crit1)
          val filter2 = criterionToFilter(crit2)
          msg => filter1(msg) || filter2(msg)
        case Recent => _.flags.contains(Imap.Flag.Recent)
        case Seen => _.flags.contains(Imap.Flag.Seen)
        case SentBefore(date) =>
          _.headers(MailHeaders.Date).map(_.toLocalDate.isBefore(date)).getOrElse(false)
        case SentOn(date) =>
          _.headers(MailHeaders.Date).map(_.toLocalDate.equals(date)).getOrElse(false)
        case SentSince(date) =>
          _.headers(MailHeaders.Date).map(!_.toLocalDate.isBefore(date)).getOrElse(false)
        case Since(date) => !_.internalDate.toLocalDate.isBefore(date)
        case Smaller(size) => _.size < size
        case Subject(str) =>
          _.headers(MailHeaders.Subject).map(_.contains(str)).getOrElse(false)
        case Text(str) =>
          msg => msg.body.contains(str) || msg.headers.toString.contains(str)
        case To(to) =>
          _.headers(MailHeaders.To).map(_.exists(_.toString.contains(to))).getOrElse(false)
        case Uid(set) => msg => set.contains(msg.uid)
        case Unanswered => !_.flags.contains(Imap.Flag.Answered)
        case Undeleted => !_.flags.contains(Imap.Flag.Deleted)
        case Undraft => !_.flags.contains(Imap.Flag.Draft)
        case Unflagged => !_.flags.contains(Imap.Flag.Flagged)
        case Unkeyword(flag) => !_.flags.contains(flag)
        case Unseen => !_.flags.contains(Imap.Flag.Seen)
      }
    }
  }
  
  class InMemoryMessage(
    @volatile var mailbox: InMemoryMailbox,
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
          Imap.BodyStructureSingleExtension(
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
    
    def markSeen(): Future[Unit] = Future.successful {
      flags += Imap.Flag.Seen
      flags -= Imap.Flag.Recent
    }
    
    def getPart(part: Seq[Int]): Future[Option[String]] = {
      // Empty means all
      if (part.isEmpty) {
        Future.successful(Some(headers.toString() + "\r\n\r\n" + body))
      } else {
        println("Not implemented yet for part: " + part)
        ???
      }
    }
    
    def getHeader(part: Seq[Int], name: String): Future[Seq[String]] = {
      if (!part.isEmpty) { println("NO4"); ??? }
      else Future.successful(MailHeaders.typeFromString(name).toSeq.flatMap(headers.lines(_)))
    }
      
    def getHeaders(part: Seq[Int], notIncluding: Seq[String]): Future[Seq[String]] = {
      val not = notIncluding.flatMap(MailHeaders.typeFromString(_).toSeq)
      Future.successful(headers.headers.keys.filterNot(not.contains).flatMap(headers.lines(_)).toSeq)
    }
    
    def getMime(part: Seq[Int]): Future[Option[String]] = { println("NO5"); ??? }
    
    def getText(part: Seq[Int], offset: Option[Int], count: Option[Int]): Future[Option[String]] = {
      if (!part.isEmpty) { println("NO5"); ??? }
      else if (offset.isEmpty) Future.successful(Some(body))
      else if (offset.get >= body.length) Future.successful(Some(""))
      else Future.successful(
        Some(body.substring(offset.get, Math.min(body.length, offset.get + count.getOrElse(body.length))))
      )
    }
      
    def alterFlags(flags: Set[Imap.Flag], operation: Imap.FlagOperation): Future[Unit] = {
      // TODO: We need to ignore keyword flags until we are ready to support some form of PERMANENTFLAGS
      val flagSet = flags // flags.filterNot(_.isInstanceOf[Imap.Flag.NonStandard])
      operation match {
        case Imap.FlagOperation.Replace => this.flags = flagSet
        case Imap.FlagOperation.Add => this.flags ++= flagSet
        case Imap.FlagOperation.Remove => this.flags --= flagSet
      }
      Future.successful(())
    }
  }
}