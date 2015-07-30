package smail
package imap
package handler

import java.time.OffsetDateTime
import scala.concurrent.Future
import java.util.regex.Pattern
import smail.Util
import scala.util.Try
import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success

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
    println("ALL NAMES PRE CREATE: " + allFolderNames("", currentUser.get.folders.values).keys.toSeq)
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
      val foundChild = parent match {
        case Some(parent) => parent._children.find(_.name == name)
        case None => currentUser.get.folders.get(name)
      }
      Some(foundChild.getOrElse {
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
    println("ALL FOLDER NAMES: " + allFolders.keys.toSeq)
    
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
      ListItem(name.stripPrefix("/"), attrs)
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
    @volatile var permanentFlags: Set[Imap.Flag] = Imap.Flag.StandardFlags,
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
    
    def getMessages(start: BigInt, end: Option[BigInt], byUid: Boolean): Future[Seq[(BigInt, MailboxMessage)]] = {
      Future.successful(getInMemoryMessages(start, end, byUid))
    }
    
    def addMessage(message: String, flags: Set[Imap.Flag], date: OffsetDateTime): Future[Option[String]] = {
      val msg: Message = Message.fromString(message) match {
        case Failure(t) => return Future.failed(t)
        case Success(v) => v
      }
      addMessage(
        new InMemoryMessage(
          mailbox = this,
          uid = nextUid,
          flags = flags,
          headers = msg.fields,
          body = msg.body.getOrElse(""),
          internalDate = date
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
      def toStringCiContains(toSearchFor: String, obj: Object): Boolean =
        obj.toString.toLowerCase.contains(toSearchFor.toLowerCase)
      def headerContainsCi[T <: Message.Field : ClassTag](
        toSearchFor: String
      ): (InMemoryMessage => Boolean) = {
        msg => msg.headerList[T].exists(h => toStringCiContains(toSearchFor, h.valueToString()))
      }
      criterion match {
        case SequenceSet(set) => set match {
          case Imap.SequenceSet(Seq(Imap.SequenceNumberAll)) =>
            // Special case where only the last one matches
            msg => messages.lastOption == Some(msg)
          case _ =>
            msg => set.contains(messages.indexOf(msg) + 1)
        }
        case All => _ => true
        case Answered => _.flags.contains(Imap.Flag.Answered)
        case Bcc(bcc) => headerContainsCi[Message.Field.Bcc](bcc)
        case Before(date) => _.internalDate.toLocalDate.isBefore(date)
        case Body(str) => msg => toStringCiContains(str, msg.body)
        case Cc(cc) => headerContainsCi[Message.Field.Cc](cc)
        case Deleted => _.flags.contains(Imap.Flag.Deleted)
        case Draft => _.flags.contains(Imap.Flag.Draft)
        case Flagged => _.flags.contains(Imap.Flag.Flagged)
        case From(from) => headerContainsCi[Message.Field.From](from)
        case Header(name, v) =>
          msg => msg.headers.exists(h =>
            h.prefix.equalsIgnoreCase(name) && toStringCiContains(v, h.valueToString())
          )
        case Keyword(flag) => _.flags.contains(flag)
        case Larger(size) => _.size > size
        case New =>
          println("MSGS: " + messages.map(_.flags))
          msg => msg.flags.contains(Imap.Flag.Recent) && !msg.flags.contains(Imap.Flag.Seen)
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
          _.headerList[Message.Field.OrigDate].exists(_.value.toLocalDate.isBefore(date))
        case SentOn(date) =>
          _.headerList[Message.Field.OrigDate].exists(_.value.toLocalDate.equals(date))
        case SentSince(date) =>
          _.headerList[Message.Field.OrigDate].exists(!_.value.toLocalDate.isBefore(date))
        case Since(date) => !_.internalDate.toLocalDate.isBefore(date)
        case Smaller(size) => _.size < size
        case Subject(str) =>
          headerContainsCi[Message.Field.Subject](str)
        case Text(str) =>
          msg => toStringCiContains(str, msg.body) || { val f = headerContainsCi[Message.Field](str); f(msg) }
        case To(to) => headerContainsCi[Message.Field.To](to)
        case Uid(set) => set match {
          case Imap.SequenceSet(Seq(Imap.SequenceNumberAll)) =>
            // Special case where only the last one matches
            msg => messages.lastOption == Some(msg)
          case _ => set.items.lastOption match {
            case Some(
              Imap.SequenceRange(Imap.SequenceNumberLiteral(low), Imap.SequenceNumberAll)
            ) if !messages.exists(_.uid >= low) =>
              // Special case where we have to include the last one
              msg => messages.lastOption == Some(msg)
            case _ =>
              msg => set.contains(msg.uid)
          }
            
        }
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
    @volatile var headers: Seq[Message.Field],
    @volatile var body: String,
    val internalDate: OffsetDateTime = OffsetDateTime.now()
  ) extends MailboxMessage {
    
    def headerList[T <: Message.Field : ClassTag]: Seq[T] = {
      val cls = implicitly[ClassTag[T]].runtimeClass
      headers.filter(h => cls.isAssignableFrom(h.getClass)).map(_.asInstanceOf[T])
    }
    
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
        date = headerList[Message.Field.OrigDate].headOption.map(_.value),
        // TODO: what about multiple subjects?
        subject = headerList[Message.Field.Subject].headOption.map(_.value),
        from = headerList[Message.Field.From].flatMap(_.value),
        sender = headerList[Message.Field.Sender].map(_.value),
        replyTo = headerList[Message.Field.ReplyTo].flatMap(_.value),
        to = headerList[Message.Field.To].flatMap(_.value),
        cc = headerList[Message.Field.Cc].flatMap(_.value),
        bcc = headerList[Message.Field.Bcc].flatMap(_.value),
        inReplyTo = headerList[Message.Field.InReplyTo].flatMap(_.value),
        messageId = headerList[Message.Field.MessageId].headOption.map(_.value)
      )
    }
    
    // TODO: confirm whether this is just body or not
    def size: BigInt = body.length
    
    def markSeen(): Future[Unit] = Future.successful {
      flags += Imap.Flag.Seen
      flags -= Imap.Flag.Recent
    }
    
    def getPart(part: Seq[Int]): Future[Option[String]] = {
      // Empty means all
      if (part.isEmpty) {
        Future.successful(Some(headers.mkString("\r\n") + "\r\n\r\n" + body))
      } else {
        println("Not implemented yet for part: " + part)
        ???
      }
    }
    
    def getHeader(part: Seq[Int], name: String): Future[Seq[String]] = {
      if (!part.isEmpty) { println("NO4"); ??? }
      else Future.successful(headers.filter(_.prefix.equalsIgnoreCase(name)).map(_.toString))
    }
      
    def getHeaders(part: Seq[Int], notIncluding: Seq[String]): Future[Seq[String]] = {
      val ret = headers.filterNot(h => notIncluding.exists(_.equalsIgnoreCase(h.prefix)))
      Future.successful(ret.map(_.toString))
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
      val flagSet = flags //flags.filterNot(_.isInstanceOf[Imap.Flag.NonStandard])
      operation match {
        case Imap.FlagOperation.Replace =>
          // Note, we do NOT replace Recent here
          val hadRecent = this.flags.contains(Imap.Flag.Recent)
          this.flags = flagSet
          if (hadRecent) this.flags += Imap.Flag.Recent
        case Imap.FlagOperation.Add => this.flags ++= flagSet
        case Imap.FlagOperation.Remove => this.flags --= flagSet
      }
      Future.successful(())
    }
  }
}