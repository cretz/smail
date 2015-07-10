package scimap
package handler

import scala.collection.immutable.NumericRange
import scala.collection.immutable.TreeSet
import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class HighLevelServerHandler(val server: HighLevelServer)
    (implicit val execCtx: ExecutionContext) extends ServerHandler {
  import HighLevelServerHandler._
  import HighLevelServer._
  import scimap.{ServerResponse => ser, ClientCommand => cli}
  
  // We accept the cheapness of volatile since state change in IMAP is not really racy anyways
  @volatile var state = State.Started
  @volatile var pendingAuthentication = Option.empty[cli.Authenticate]
  
  override def handle(res: Option[cli.ParseResult]): Option[Future[Seq[ser]]] = {
    if (state != State.Started && res.isEmpty) return None
    else return Some(handleOption(res))
  }
  
  // TODO: lots of cleanup needed here
  def handleOption(res: Option[cli.ParseResult]): Future[Seq[ser]] = {
    (state, res) match {
      case (State.Started, None) =>
        handleFirstConnect()
      case (State.Started, _) =>
        failAndClose("Client sent first command")
      case (_, None) =>
        // This means that no command was parsed...TODO: handle server-initiated update
        Future.successful(Seq.empty)
      case (_, Some(cli.CommandSuccess(cli.Logout(tag)))) =>
        handleLogout(tag)
      case (State.NotAuthenticated, Some(cli.CommandSuccess(auth: cli.Authenticate))) =>
        println("AUTH, YAY!")
        requestAuthentication(auth)
      case (State.NotAuthenticated, Some(cli.UnrecognizedCommand(seq))) if pendingAuthentication.isDefined =>
        handlePendingAuthentication(seq)
      case (State.NotAuthenticated, Some(cli.CommandSuccess(cli.StartTls(tag)))) =>
        Future.successful(Seq(ser.Ok("Begin TLS negotiation now", Some(tag)), ser.StartTls))
      case (State.NotAuthenticated, _) =>
        failAndClose("Not authenticated")
      case (_, Some(cli.CommandSuccess(cli.Capability(tag)))) =>
        handleCapabilities(tag)
      case (s, Some(cli.CommandSuccess(cli.Examine(tag, mailbox)))) if s >= State.Authenticated =>
        handleSelectOrExamine(tag, mailbox, false)
      case (s, Some(cli.CommandSuccess(cli.Select(tag, mailbox)))) if s >= State.Authenticated =>
        handleSelectOrExamine(tag, mailbox, true)
      case (_, Some(cli.CommandSuccess(cli.Noop(tag)))) =>
        handleNoop(tag)
      case (_, Some(cli.CommandSuccess(cli.List(tag, reference, mailbox)))) =>
        handleList(tag, reference, mailbox)
      case (State.Selected, Some(cli.CommandSuccess(fetch: cli.Fetch))) =>
        handleFetch(fetch)
      case (State.Selected, Some(cli.CommandSuccess(cli.Close(tag)))) =>
        handleClose(tag)
      case v =>
        failAndClose("Unknown state/command: " + v)
    }
  }
  
  def handleFirstConnect(): Future[Seq[ser]] = {
      // Call this before state change so implementers can know if this was in response to a request or not
      server.capabilities().map { caps =>
        state = State.NotAuthenticated
        Seq(ser.Ok("Service ready", None, Some(ser.StatusResponseCode.Capability(caps))))
      }
  }
  
  def handleLogout(tag: String): Future[Seq[ser]] = {
    server.close().map { _ =>
      state = State.Logout
      Seq(ser.Bye("Server logging out"), ser.Ok("LOGOUT completed", Some(tag)), ser.CloseConnection)
    }
  }
  
  def handleCapabilities(tag: String): Future[Seq[ser]] = {
    server.capabilities().map { caps =>
      Seq(ser.Capability(caps), ser.Ok("Complete", Some(tag)))
    }
  }
  
  def requestAuthentication(auth: cli.Authenticate): Future[Seq[ser]] = {
    if (pendingAuthentication.isDefined) failAndClose("Authentication already pending")
    else if (auth.mechanism != "PLAIN")
      Future.successful(Seq(ser.No("Only PLAIN accepted", Some(pendingAuthentication.get.tag))))
    else {
      pendingAuthentication = Some(auth)
      Future.successful(Seq(ser.Continuation()))
    }
  }
  
  def handlePendingAuthentication(tokens: Seq[ImapToken]): Future[Seq[ser]] = {
    if (!pendingAuthentication.isDefined || pendingAuthentication.get.mechanism != "PLAIN")
      return Future.successful(Seq(ser.No("Only PLAIN accepted")))
    val tag = pendingAuthentication.get.tag
    pendingAuthentication = None
    tokens match {
      case Seq(ImapToken.Str(userPass, _)) =>
        // Per RFC-2595, this is a base 64'd string of the null char + UTF-8 user/pass separated by a null char
        val bytes = Util.base64Decode(userPass)
        if (bytes.headOption != Some(0)) Future.successful(Seq(ser.Bad("Unable to read credentials", Some(tag))))
        else bytes.tail.indexOf(0) match {
          case -1 => Future.successful(Seq(ser.Bad("Unable to read credentials", Some(tag))))
          case idx => bytes.tail.splitAt(idx) match {
            case (usernameBytes, passwordBytes) =>
              val username = new String(usernameBytes, "UTF-8")
              server.authenticatePlain(username, new String(passwordBytes.tail, "UTF-8")).map { success =>
                if (success) {
                  state = State.Authenticated
                  Seq(ser.Ok("Login successful", Some(tag)))
                } else Seq(ser.No("Login failed", Some(tag)))
              }
            case arr =>
              Future.successful(Seq(ser.Bad("Unable to read credentials", Some(tag))))
          }
        }
      case _ => Future.successful(Seq(ser.Bad("Unable to read credentials", Some(tag))))
    }
  }
  
  def handleSelectOrExamine(tag: String, mailbox: String, isSelect: Boolean): Future[Seq[ser]] = {
    server.select(mailbox, !isSelect).map {
      case None => Seq(ser.No("Mailbox not found", Some(tag)))
      case Some(result) =>
        state = State.Selected
        val okInfo =
          if (isSelect) "SELECT" -> ser.StatusResponseCode.ReadWrite
          else "EXAMINE" -> ser.StatusResponseCode.ReadOnly
        Seq(
          ser.Exists(result.exists),
          ser.Recent(result.recent),
          ser.Ok(
            "Message " + result.firstUnseen + " is first unseen", 
            None,
            Some(ser.StatusResponseCode.Unseen(result.firstUnseen))
          ),
          ser.Flags(result.flags.toSeq),
          ser.Ok("Permanent flags", None, Some(ser.StatusResponseCode.PermanentFlags(result.permanentFlags.toSeq))),
          ser.Ok("UIDs valid", None, Some(ser.StatusResponseCode.UidValidity(result.uidValidity))),
          ser.Ok("Predicted next UID", None, Some(ser.StatusResponseCode.UidNext(result.nextUid))),
          ser.Ok(okInfo._1 + " completed", Some(tag), Some(okInfo._2))
        )
    }
  }
  
  def handleNoop(tag: String): Future[Seq[ser]] = {
    var responses = Seq.empty[ser]
    server.currentMailbox.foreach { res =>
      responses :+= ser.Exists(res.exists)
      responses :+= ser.Recent(res.recent)
      // TODO: EXPUNGE and FETCH
    }
    Future.successful(responses :+ ser.Ok("NOOP completed", Some(tag)))
  }
  
  def listStringToTokens(str: String): Seq[Imap.ListToken] = {
    // Get first wildcard path
    val (wildcard, index) = str.indexOf('%') -> str.indexOf('*') match {
      case (-1, -1) => return Seq(Imap.ListToken.Str(str))
      case (-1, b) => Imap.ListToken.PathWildcard -> b
      case (a, -1) => Imap.ListToken.NameWildcard -> a
      case (a, b) if a < b => Imap.ListToken.NameWildcard -> a
      case (a, b) => Imap.ListToken.PathWildcard -> b
    }
    // Split it up recursively
    str.splitAt(index) match {
      case ("", rhs) if rhs.length == 1 => Seq(wildcard)
      case (lhs, rhs) if rhs.length == 1 => Seq(Imap.ListToken.Str(lhs), wildcard)
      case ("", rhs) => wildcard +: listStringToTokens(rhs.tail)
      case (lhs, rhs) => Seq(Imap.ListToken.Str(lhs), wildcard) ++ listStringToTokens(rhs.tail)
    }
  }
  
  def handleList(tag: String, reference: String, mailbox: String): Future[Seq[ser]] = {
    val delim = server.hierarchyDelimiter
    // If the mailbox is empty, all we need is the delimiter
    if (mailbox.isEmpty) return Future.successful(
      Seq(ser.List("", delim, Seq(Imap.ListAttribute.NoSelect)), ser.Ok("LIST Completed", Some(tag)))
    )
    // Break apart the pieces
    val startsAtRoot = server.hierarchyRoots.exists(s => reference.startsWith(s) || mailbox.startsWith(s))
    val combined = if (server.hierarchyRoots.exists(mailbox.startsWith)) mailbox else reference + mailbox
    val pieces = delim match {
      case Some(delim) => combined.split(Pattern.quote(delim)).toSeq
      case None => Seq(combined)
    }
    // Go over each and create token sets
    val tokenSets = pieces.map(listStringToTokens)
    // Ask server for the list
    server.list(tokenSets, startsAtRoot).map { list =>
      list.map(i => ser.List(i.path, delim, i.attrs)) :+ ser.Ok("LIST Completed", Some(tag))
    }
  }
  
  def handleFetch(fetch: cli.Fetch): Future[Seq[ser]] = {
    val mailbox = server.currentMailbox.getOrElse(
      return Future.successful(Seq(ser.Bad("No mailbox selected", Some(fetch.tag))))
    )
    
    // Get proper sequences to fetch
    val ranges = fetch.set.items.map {
      case num: Imap.SequenceNumber => num.valueOption.getOrElse(BigInt(1)) -> num.valueOption
      case Imap.SequenceRange(low, high) => low.valueOption.getOrElse(BigInt(1)) -> high.valueOption
    }
    
    // Get the real items we are being asked for
    val items = fetch.dataItems match {
      case Left(cli.FetchMacro.All) =>
        Seq(cli.FetchDataItem.Flags, cli.FetchDataItem.InternalDate,
          cli.FetchDataItem.Rfc822Size, cli.FetchDataItem.Envelope)
      case Left(cli.FetchMacro.Fast) =>
        Seq(cli.FetchDataItem.Flags, cli.FetchDataItem.InternalDate,
          cli.FetchDataItem.Rfc822Size)
      case Left(ClientCommand.FetchMacro.Full) =>
        Seq(cli.FetchDataItem.Flags, cli.FetchDataItem.InternalDate, cli.FetchDataItem.Rfc822Size,
          cli.FetchDataItem.Envelope, cli.FetchDataItem.Body(Imap.BodyPart.Part(Seq.empty)))
      case Right(seq) => seq
    }
    
    // Go over each range and ask for messages (right side of either is failure)
    // TODO: This needs to be streaming/lazy, not chunked
    type GetMessageResult = Either[Seq[(BigInt, Message)], String]
    val msgFutures: Set[Future[GetMessageResult]] = Util.bigIntGaps(ranges).map { range =>
      val end = range._2.getOrElse(mailbox.exists)
      mailbox.getMessages(range._1, end).map {
        // Any none means error
        // TODO: find a way to make this short-circuit the other work
        case None =>
          Right("Unable to read message")
        case Some(newMsgs) if newMsgs.size != (end - range._1 + 1).toInt =>
          Right("Unable to find all messages")
        case Some(newMsgs) =>
          Left((range._1 to end).zip(newMsgs))
      }
    }
    
    // Fold them all into one set (left is ok, right is bad)
    type AllMessagesResult = Either[Future[Seq[ser]], Seq[ser]]
    val msgs = Future.fold(msgFutures)(Left(Future.successful(Seq.empty[ser])): AllMessagesResult) {
      // Failure (right side) means do nothing else
      case (r @ Right(_), _) => r
      case (_, Right(err)) => Right(Seq(ser.Bad(err, Some(fetch.tag))))
      case (Left(futureSeq), Left(msgs)) =>
        val thisSet = Future.sequence(msgs.map { case (seq, msg) =>
          messageToFetchItems(msg, items).map { fetchItems =>
            if (fetchItems.isEmpty) Seq.empty else Seq(ser.Fetch(seq, fetchItems))
          }
        }).map(_.flatten)
        Left(futureSeq.flatMap(seq => thisSet.map(seq ++ _)))
    }
    
    msgs.flatMap {
      case Right(badSeq) => Future.successful(badSeq)
      case Left(goodFutureSeqs) =>
        goodFutureSeqs.map(_ :+ ser.Ok("FETCH completed", Some(fetch.tag)))
    }
  }
  
  def messageToFetchItems(msg: Message, itemsRequested: Seq[cli.FetchDataItem]): Future[Seq[ser.FetchDataItem]] = {
    val items = itemsRequested.map {
      case cli.FetchDataItem.NonExtensibleBodyStructure =>
        Future.successful(
          Seq(ser.FetchDataItem.NonExtensibleBodyStructure(bodyStructureToImapToken(msg.bodyStructure.sansExtension)))
        )
      case cli.FetchDataItem.Body(part, offset, count) =>
        fetchBodyPart(msg, part, offset, count).flatMap { ret =>
          msg.markSeen().map(_ => ret.toSeq)
        }
      case cli.FetchDataItem.BodyPeek(part, offset, count) =>
        fetchBodyPart(msg, part, offset, count).map(_.toSeq)
      case cli.FetchDataItem.BodyStructure =>
        Future.successful(Seq(ser.FetchDataItem.BodyStructure(bodyStructureToImapToken(msg.bodyStructure))))
      case cli.FetchDataItem.Envelope =>
        Future.successful(Seq(ser.FetchDataItem.Envelope(envelopeToImapToken(msg.envelope))))
      case cli.FetchDataItem.Flags =>
        Future.successful(Seq(ser.FetchDataItem.Flags(msg.flags.toSeq)))
      case cli.FetchDataItem.InternalDate =>
        Future.successful(Seq(ser.FetchDataItem.InternalDate(msg.internalDate)))
      case cli.FetchDataItem.Rfc822 =>
        msg.getHeaders(Seq.empty, Seq.empty).flatMap { headers =>
          msg.getText(Seq.empty, None, None).map { text =>
            Seq(ser.FetchDataItem.Rfc822(headers.mkString("\r\n") + "\r\n\r\n" + text.getOrElse("")))
          }
        }
      case cli.FetchDataItem.Rfc822Header =>
        msg.getHeaders(Seq.empty, Seq.empty).map { headers =>
          Seq(ser.FetchDataItem.Rfc822Header(headers.mkString("\r\n") + "\r\n"))
        }
      case cli.FetchDataItem.Rfc822Size =>
        Future.successful(Seq(ser.FetchDataItem.Rfc822Size(msg.size)))
      case cli.FetchDataItem.Rfc822Text =>
        msg.getText(Seq.empty, None, None).map { text =>
          text.map(ser.FetchDataItem.Rfc822Text(_)).toSeq
        }
      case cli.FetchDataItem.Uid =>
        Future.successful(Seq(ser.FetchDataItem.Uid(msg.uid)))
    }
    Future.sequence(items).map(_.flatten)
  }
  
  def fetchBodyPart(
    msg: HighLevelServer.Message,
    part: Imap.BodyPart,
    offset: Option[Int] = None,
    count: Option[Int] = None
  ): Future[Option[ser.FetchDataItem.Body]] = {
    @inline
    def substr(str: String) =
      if (!offset.isDefined) str
      else str.substring(offset.get, Math.min(str.length, count.getOrElse(str.length) - offset.get))
    def headerLinesToStr(lines: Seq[String]) =
      if (lines.isEmpty) None
      else Some(substr(lines.mkString("\r\n")) + "\r\n") // TODO: where to substr this?
    part match {
      case Imap.BodyPart.Part(nums) =>
        msg.getPart(nums).map {
          _.map(s => ser.FetchDataItem.Body(part, substr(s), offset))
        }
      case Imap.BodyPart.Header(nums) =>
        msg.getHeaders(nums, Seq.empty).map {
          headerLinesToStr(_).map(s => ser.FetchDataItem.Body(part, s, offset))
        }
      case Imap.BodyPart.HeaderFields(nums, fields) =>
        Future.sequence(fields.map(msg.getHeader(nums, _))).map(_.flatten).map {
          headerLinesToStr(_).map(s => ser.FetchDataItem.Body(part, s, offset))
        }
      case Imap.BodyPart.HeaderFieldsNot(nums, fields) =>
        msg.getHeaders(nums, fields).map {
          headerLinesToStr(_).map(s => ser.FetchDataItem.Body(part, s, offset))
        }
      case Imap.BodyPart.Mime(nums) =>
        msg.getMime(nums).map {
          _.map(s => ser.FetchDataItem.Body(part, substr(s), offset))
        }
      case Imap.BodyPart.Text(nums) =>
        msg.getText(nums, offset, count).map {
          _.map(s => ser.FetchDataItem.Body(part, s, offset))
        }
    }
  }

  def bodyStructureToImapToken(structure: Imap.BodyStructure): ImapToken.List = {
    import ImapToken._
    @inline
    def mapToToken(map: Map[String, String]): ImapToken =
      if (map.isEmpty) Nil
      else List('(', map.map(v => Seq(Str(v._1, true), Str(v._2, true))).flatten.toSeq)
    structure match {
      case Imap.BodyStructureMulti(parts, subType, extension) =>
        // TODO:
        ???
      case str: Imap.BodyStructureSingle =>
        var list = List('(', Seq(
          Str(str.bodyType, true),
          Str(str.subType, true),
          mapToToken(str.parameters),
          str.id.map(Str(_, true)).getOrElse(Nil),
          str.description.map(Str(_, true)).getOrElse(Nil),
          str.encoding.map(Str(_, true)).getOrElse(Nil),
          Str(str.size.toString)
        ))
        // Add extra things?
        var addToList = Seq.empty[ImapToken]
        // Lines only get added if present
        str.lineCount.foreach(v => addToList :+= Str(v.toString))
        // Same with extension
        str.extension.foreach { ext =>
          if (!ext.md5.isEmpty || !ext.disposition.isEmpty || !ext.language.isEmpty || !ext.location.isEmpty) {
            addToList :+= ext.md5.map(Str(_, true)).getOrElse(Nil)
            if (!ext.disposition.isEmpty || !ext.language.isEmpty || !ext.location.isEmpty) {
              addToList ++= ext.disposition.map({ case (name, vals) =>
                Seq(Str(name, true), mapToToken(vals))
              }).getOrElse(Seq(Nil))
              if (!ext.language.isEmpty || !ext.location.isEmpty) {
                addToList :+= (ext.language match {
                  case Seq() => Nil
                  case Seq(single) => Str(single, true)
                  case multi => List('(', multi.map(Str(_, true)))
                })
                if (!ext.location.isEmpty) addToList :+= List('(', ext.location.map(Str(_, true)))
              }
            }
          }
        }
        list.copy(values = list.values ++ addToList)
      case _ => ???
    }
  }
  
  def envelopeToImapToken(env: Imap.Envelope): ImapToken.List = {
    import ImapToken._
    @inline
    def addrsOrNil(addrs: Seq[Imap.MailAddress]): ImapToken =
      if (addrs.isEmpty) Nil else List('(', addrs.map(mailAddressToImapToken))
    List('(', Seq(
      env.date.map(d => Str(Imap.mailDateTimeFormat.format(d), true)).getOrElse(Nil),
      env.subject.map(Str(_, true)).getOrElse(Nil),
      addrsOrNil(env.from),
      addrsOrNil(env.sender),
      addrsOrNil(env.replyTo),
      addrsOrNil(env.to),
      addrsOrNil(env.cc),
      addrsOrNil(env.bcc),
      if (env.inReplyTo.isEmpty) Nil else List('(', env.inReplyTo.map(m => Str(m._1 + "@" + m._2, true))),
      env.messageId.map(m => Str(m._1 + "@" + m._2, true)).getOrElse(Nil)
    ))
  }
  
  def mailAddressToImapToken(addr: Imap.MailAddress): ImapToken.List = {
    import ImapToken._
    addr match {
      case Imap.MailboxAddress((mailbox, host), personalName) =>
        // Ignore SMTP "at-domain-list"
        List('(', Seq(personalName.map(Str(_, true)).getOrElse(Nil), Nil, Str(mailbox, true), Str(host, true)))
      case Imap.GroupAddress(display, mailboxes) =>
        List('(', Str(display, true) +:
          mailboxes.map(mailAddressToImapToken) :+ List('(', Seq(Str(display, true), Nil, Nil, Nil)))
    }
  }
  
  def handleClose(tag: String): Future[Seq[ser]] = {
    server.flushCurrentMailboxDeleted().flatMap { _ =>
      server.closeCurrentMailbox().map { _ =>
        state = State.Authenticated
        Seq(ser.Ok("CLOSE completed", Some(tag)))
      }
    }
  }
}
object HighLevelServerHandler {
  object State extends Enumeration {
    val Started = Value
    val NotAuthenticated = Value
    val Authenticated = Value
    val Selected = Value
    val Logout = Value
  }
}