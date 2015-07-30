package smail
package imap
package handler

import scala.collection.immutable.NumericRange
import scala.collection.immutable.TreeSet
import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import smail.Util
import java.time.OffsetDateTime

class HighLevelServerHandler(val server: HighLevelServer)
    (implicit val execCtx: ExecutionContext) extends ServerHandler {
  import HighLevelServerHandler._
  import HighLevelServer._
  import smail.imap.{ServerResponse => ser, ClientCommand => cli}
  
  // We accept the cheapness of volatile since state change in IMAP is not really racy anyways
  @volatile var state = State.Started
  @volatile var pendingAuthentication = Option.empty[cli.Authenticate]
  @volatile var asyncCallback: Seq[ServerResponse] => Unit = { _ => ??? }
  @volatile var currentlyRunningIdleTagAndUnsetter = Option.empty[(String, () => Unit)]
  
  override def registerOutOfBandCallback(cb: Seq[ServerResponse] => Unit): Unit = asyncCallback = cb
  
  override def handle(res: Option[cli.ParseResult]): Option[Future[Seq[ser]]] = {
    if (state != State.Started && res.isEmpty) None
    else Some(handleOption(res))
  }
  
  // TODO: lots of cleanup needed here
  def handleOption(res: Option[cli.ParseResult]): Future[Seq[ser]] = {
    // If we're in idle, the only thing we accept from a client is DONE
    if (currentlyRunningIdleTagAndUnsetter.isDefined) res match {
      case Some(cli.UnrecognizedCommand(Seq(ImapToken.Str("DONE", _)))) =>
        return endIdle()
      case _ =>
        failAndClose("Unacceptable command in IDLE state")
    }
    // Match other situations
    (state, res) match {
      // If we are waiting on text, go ahead and accept it w/ a continuation...
      case (_, Some(cli.WaitingForMoreText(_))) =>
        Future.successful(Seq(ser.Continuation(Some("Ready for additional command text"))))
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
        requestAuthentication(auth)
      case (State.NotAuthenticated, Some(cli.UnrecognizedCommand(seq))) if pendingAuthentication.isDefined =>
        handlePendingAuthentication(seq)
      case (State.NotAuthenticated, Some(cli.CommandSuccess(cli.Login(tag, username, password)))) =>
        handleLogin(tag, username, password)
      case (State.NotAuthenticated, Some(cli.CommandSuccess(cli.StartTls(tag)))) =>
        Future.successful(Seq(ser.Ok("Begin TLS negotiation now", Some(tag)), ser.StartTls))
      case (_, Some(cli.CommandSuccess(cli.Capability(tag)))) =>
        handleCapabilities(tag)
      case (state, _) if state < State.Authenticated =>
        failAndClose("Not authenticated")
      case (_, Some(cli.CommandSuccess(cli.Select(tag, mailbox)))) =>
        handleSelectOrExamine(tag, mailbox, true)
      case (_, Some(cli.CommandSuccess(cli.Examine(tag, mailbox)))) =>
        handleSelectOrExamine(tag, mailbox, false)
      case (_, Some(cli.CommandSuccess(cli.Create(tag, mailbox)))) =>
        handleCreate(tag, mailbox)
      case (_, Some(cli.CommandSuccess(cli.Delete(tag, mailbox)))) =>
        handleDelete(tag, mailbox)
      case (_, Some(cli.CommandSuccess(cli.Rename(tag, oldName, newName)))) =>
        handleRename(tag, oldName, newName)
      case (_, Some(cli.CommandSuccess(cli.Subscribe(tag, name)))) =>
        handleSubscribe(tag, name)
      case (_, Some(cli.CommandSuccess(cli.Unsubscribe(tag, name)))) =>
        handleUnsubscribe(tag, name)
      case (_, Some(cli.CommandSuccess(cli.Noop(tag)))) =>
        handleNoop(tag)
      case (_, Some(cli.CommandSuccess(cli.List(tag, reference, mailbox)))) =>
        handleList(tag, reference, mailbox, false)
      case (_, Some(cli.CommandSuccess(cli.LSub(tag, reference, mailbox)))) =>
        handleList(tag, reference, mailbox, true)
      case (_, Some(cli.CommandSuccess(cli.Status(tag, mailbox, dataItems)))) =>
        handleStatus(tag, mailbox, dataItems)
      case (_, Some(cli.CommandSuccess(cli.Append(tag, mailbox, message, flags, date)))) =>
        handleAppend(tag, mailbox, message, flags, date)
      case (State.Selected, Some(cli.CommandSuccess(cli.Check(tag)))) =>
        handleCheck(tag)
      case (State.Selected, Some(cli.CommandSuccess(cli.Close(tag)))) =>
        handleClose(tag)
      case (State.Selected, Some(cli.CommandSuccess(cli.Expunge(tag)))) =>
        handleExpunge(tag)
      case (State.Selected, Some(cli.CommandSuccess(cli.Search(tag, criteria, charset)))) =>
        handleSearch(tag, criteria, charset)
      case (State.Selected, Some(cli.CommandSuccess(fetch: cli.Fetch))) =>
        handleFetch(fetch)
      case (State.Selected, Some(cli.CommandSuccess(cli.Store(tag, set, item)))) =>
        handleStore(tag, set, item)
      case (State.Selected, Some(cli.CommandSuccess(cli.Copy(tag, set, mailbox)))) =>
        handleCopy(tag, set, mailbox)
      case (State.Selected, Some(cli.CommandSuccess(cli.Uid(tag, command)))) =>
        handleUid(tag, command)
      case (_, Some(cli.CommandSuccess(cli.Idle(tag)))) =>
        beginIdle(tag)
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
  
  def handleLogin(tag: String, username: String, password: String): Future[Seq[ser]] = {
    if (pendingAuthentication.isDefined) failAndClose("Authentication already pending")
    else {
      server.authenticatePlain(username, password).map {
        case true =>
          state = State.Authenticated
          Seq(ser.Ok("Login successful", Some(tag)))
        case false =>
          Seq(ser.No("Login failed", Some(tag)))
      }
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
  
  def handleCreate(tag: String, name: String): Future[Seq[ser]] = {
    server.create(name).map {
      case None => Seq(ser.Ok("CREATE completed", Some(tag)))
      case Some(err) => Seq(ser.No("Cannot create: " + err, Some(tag)))
    }
  }
  
  def handleDelete(tag: String, name: String): Future[Seq[ser]] = {
    server.delete(name).map {
      case None => Seq(ser.Ok("DELETE completed", Some(tag)))
      case Some(err) => Seq(ser.No("Cannot delete: " + err, Some(tag)))
    }
  }
  
  def handleRename(tag: String, oldName: String, newName: String): Future[Seq[ser]] = {
    server.rename(oldName, newName).map {
      case None => Seq(ser.Ok("RENAME completed", Some(tag)))
      case Some(err) => Seq(ser.No("Cannot rename: " + err, Some(tag)))
    }
  }
  
  def handleSubscribe(tag: String, name: String): Future[Seq[ser]] = {
    server.subscribe(name).map {
      case true => Seq(ser.Ok("SUBSCRIBE completed", Some(tag)))
      case false => Seq(ser.No("Cannot subscribe to " + name, Some(tag)))
    }
  }
  
  def handleUnsubscribe(tag: String, name: String): Future[Seq[ser]] = {
    server.unsubscribe(name).map {
      case true => Seq(ser.Ok("UNSUBSCRIBE completed", Some(tag)))
      case false => Seq(ser.No("Cannot unsubscribe from " + name, Some(tag)))
    }
  }
  
  def handleNoop(tag: String): Future[Seq[ser]] = {
    server.getCurrentMailboxEvents().map { events =>
      val regularResponses = Seq(
        ser.Exists(server.currentMailbox.get.exists),
        ser.Recent(server.currentMailbox.get.recent)
      )
      val eventResponses = events.values.map({
        case MailboxEvent.MessageExpunged(seq) =>
          ser.Expunge(seq)
        case MailboxEvent.MessageFlagsUpdated(seq, flags) =>
          ser.Fetch(seq, Seq(ser.FetchDataItem.Flags(flags.toSeq)))
      }).toSeq
      eventResponses ++ regularResponses :+ ser.Ok("NOOP completed", Some(tag))
    }
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
  
  def handleList(tag: String, reference: String, mailbox: String, subscribedOnly: Boolean): Future[Seq[ser]] = {
    val prefix = if (subscribedOnly) "LSUB" else "LIST"
    val delim = server.hierarchyDelimiter
    // If the mailbox is empty, all we need is the delimiter
    if (mailbox.isEmpty) return Future.successful(
      Seq(ser.List("", delim, Seq(Imap.ListAttribute.NoSelect)), ser.Ok(prefix + " Completed", Some(tag)))
    )
    // Break apart the pieces
    val startsAtRoot = server.hierarchyRoots.exists(s => reference.startsWith(s) || mailbox.startsWith(s))
    val combined =
      if (server.hierarchyRoots.exists(mailbox.startsWith) || reference == "") mailbox
      else if (delim.isDefined && !reference.endsWith(delim.get)) reference + delim.get + mailbox
      else reference + mailbox
    val sansPrefix = delim.map(combined.stripPrefix).getOrElse(combined)
    val pieces = delim match {
      case Some(delim) => sansPrefix.split(Pattern.quote(delim)).toSeq
      case None => Seq(sansPrefix)
    }
    // Go over each and create token sets
    val tokens = pieces.map(listStringToTokens).foldLeft(Seq.empty[Imap.ListToken]) {
      case (Seq(), next) => next 
      case (prev, next) => (prev :+ Imap.ListToken.Delimiter) ++ next
    }
    // Ask server for the list
    server.list(tokens, startsAtRoot, subscribedOnly).map { list =>
      val vals = list.map { i =>
        if (subscribedOnly) ser.LSub(i.path, delim, i.attrs)
        else ser.List(i.path, delim, i.attrs)
      }
      vals :+ ser.Ok(prefix + " Completed", Some(tag))
    }
  }
  
  def handleStatus(tag: String, mailbox: String, dataItems: Seq[Imap.StatusDataItem]): Future[Seq[ser]] = {
    server.get(mailbox).map {
      case None => Seq(ser.No("Unable to find mailbox", Some(tag)))
      case Some(box) =>
        val info = dataItems.map { dataItem =>
          dataItem -> (dataItem match {
            case Imap.StatusDataItem.Messages => box.exists
            case Imap.StatusDataItem.Recent => box.recent
            case Imap.StatusDataItem.UidNext => box.nextUid
            case Imap.StatusDataItem.UidValidity => box.uidValidity
            case Imap.StatusDataItem.Unseen => box.unseen
          })
        }
        Seq(ser.Status(mailbox, info), ser.Ok("STATUS completed", Some(tag)))
    }
  }
  
  def handleAppend(
    tag: String,
    mailbox: String,
    message: String,
    flags: Seq[Imap.Flag],
    date: Option[OffsetDateTime]
  ): Future[Seq[ser]] = {
    server.get(mailbox).flatMap {
      case None =>
        Future.successful(
          Seq(ser.No("Mailbox " + mailbox + " does not exist", Some(tag), Some(ser.StatusResponseCode.TryCreate)))
        )
      case Some(mailbox) =>
        mailbox.addMessage(message, flags.toSet, date.getOrElse(OffsetDateTime.now())).map {
          case None =>
            // Per the dovecot tests, we return the message count here
            Seq(
              ser.Exists(mailbox.exists),
              ser.Ok("APPEND completed", Some(tag))
            )
          case Some(err) => Seq(ser.No("APPEND error: " + err, Some(tag)))
        }
    }
  }
  
  def handleCheck(tag: String): Future[Seq[ser]] = {
    server.currentMailbox.map({ mailbox =>
      mailbox.checkpoint().map(_ => Seq(ser.Ok("CHECK completed", Some(tag))))
    }).getOrElse(Future.successful(Seq(ser.Bad("No mailbox selected", Some(tag)))))
  }
  
  def handleClose(tag: String): Future[Seq[ser]] = {
    endIdle()
    server.closeCurrentMailbox().map { _ =>
      state = State.Authenticated
      Seq(ser.Ok("CLOSE completed", Some(tag)))
    }
  }
  
  def handleExpunge(tag: String): Future[Seq[ser]] = {
    server.currentMailbox.map({ mailbox =>
      mailbox.expunge().map { ids =>
        ids.map(ser.Expunge(_)) :+ ser.Ok("EXPUNGE completed", Some(tag))
      }
    }).getOrElse(Future.successful(Seq(ser.Bad("No mailbox selected", Some(tag)))))
  }
  
  def handleSearch(    
    tag: String,
    criteria: Seq[Imap.SearchCriterion],
    charset: Option[String],
    returnUids: Boolean = false
  ): Future[Seq[ser]] = {
    // TODO: handle charset
    if (charset.isDefined) return Future.successful(Seq(ser.No("Charsets not supported", Some(tag))))
    val prefix = if (returnUids) "UID " else ""
    server.currentMailbox.map({ mailbox =>
      mailbox.search(criteria, returnUids).map { ids =>
        Seq(ser.Search(ids), ser.Ok(prefix + "SEARCH completed", Some(tag)))
      }
    }).getOrElse(Future.successful(Seq(ser.Bad("No mailbox selected", Some(tag)))))
  }
  
  def getRangesFromSequenceSet(set: Imap.SequenceSet): Seq[(BigInt, Option[BigInt])] = {
    set.items.map {
      case num: Imap.SequenceNumber =>
        num.valueOption.getOrElse(BigInt(1)) -> num.valueOption
      case Imap.SequenceRange(Imap.SequenceNumberAll, low) =>
        low.valueOption.getOrElse(BigInt(1)) -> None
      case Imap.SequenceRange(Imap.SequenceNumberLiteral(low), Imap.SequenceNumberLiteral(high)) if high < low =>
        high -> Some(low)
      case Imap.SequenceRange(low, high) =>
        low.valueOption.getOrElse(BigInt(1)) -> high.valueOption
    }
  }
  
  def getMessagesFromSequenceSet(
      mailbox: Mailbox,
      set: Imap.SequenceSet,
      byUid: Boolean
  ): Future[Seq[(BigInt, MailboxMessage)]] = {
    val msgFutures = getRangesFromSequenceSet(set).map { range =>
      mailbox.getMessages(range._1, range._2, byUid)
    }
    Future.sequence(msgFutures.toSeq).map(_.flatten)
  }
  
  def handleFetch(fetch: cli.Fetch, byUid: Boolean = false): Future[Seq[ser]] = {
    val mailbox = server.currentMailbox.getOrElse(
      return Future.successful(Seq(ser.Bad("No mailbox selected", Some(fetch.tag))))
    )
    
    // Get the real items we are being asked for
    val unorderedItems = fetch.dataItems match {
      case Left(cli.FetchMacro.All) =>
        Seq(cli.FetchDataItem.Flags, cli.FetchDataItem.InternalDate,
          cli.FetchDataItem.Rfc822Size, cli.FetchDataItem.Envelope)
      case Left(cli.FetchMacro.Fast) =>
        Seq(cli.FetchDataItem.Flags, cli.FetchDataItem.InternalDate,
          cli.FetchDataItem.Rfc822Size)
      case Left(ClientCommand.FetchMacro.Full) =>
        Seq(cli.FetchDataItem.Flags, cli.FetchDataItem.InternalDate, cli.FetchDataItem.Rfc822Size,
          cli.FetchDataItem.Envelope, cli.FetchDataItem.NonExtensibleBodyStructure)
      case Right(seq) => seq
    }
    // Flags have to come at the end because we could affect the flags with the other items
    val items = unorderedItems.sortWith { case (lhs, _) => lhs != cli.FetchDataItem.Flags }
    
    val msgFutures = getMessagesFromSequenceSet(mailbox, fetch.set, byUid)
    val itemFutures = msgFutures.flatMap { msgs =>
     Future.sequence(msgs.map { case (seq, msg) =>
       messageToFetchItems(msg, items).map(seq -> _)
     })
    }
    val prefix = if (byUid) "UID " else ""
    itemFutures.map { msgItems =>
      msgItems.flatMap({
        case (_, Seq()) => None
        case (seq, items) => Some(ser.Fetch(seq, items))
      }) :+ ser.Ok(prefix + "FETCH completed", Some(fetch.tag))
    }
  }
  
  def handleStore(
    tag: String,
    set: Imap.SequenceSet,
    dataItem: cli.StoreDataItem,
    byUid: Boolean = false
  ): Future[Seq[ser]] = {
    val mailbox = server.currentMailbox.getOrElse(
      return Future.successful(Seq(ser.Bad("No mailbox selected", Some(tag))))
    )
    val msgFutures = getMessagesFromSequenceSet(mailbox, set, byUid)
    val responses = msgFutures.flatMap { msgs =>
      Future.sequence(msgs.map { case (seq, msg) =>
        msg.alterFlags(dataItem.flags.toSet, dataItem.operation).flatMap { _ =>
          if (dataItem.silent) Future.successful(None)
          else messageToFetchItems(msg, Seq(cli.FetchDataItem.Flags)).map {
            case Seq() => None
            case items => Some(ser.Fetch(seq, items))
          }
        }
      }).map(_.flatten)
    }
    val prefix = if (byUid) "UID " else ""
    responses.map(s => s :+ ser.Ok(prefix + "STORE completed", Some(tag)))
  }
  
  def handleCopy(
    tag: String,
    set: Imap.SequenceSet,
    mailbox: String,
    byUid: Boolean = false
  ): Future[Seq[ser]] = {
    val futures = getRangesFromSequenceSet(set).map({ range =>
      server.copyMessagesFromCurrent(range._1, range._2, mailbox, byUid)
    }).toSeq
    Future.sequence(futures).map { errors =>
      val prefix = if (byUid) "UID " else ""
      errors.find(_.isDefined).flatten match {
        case None => Seq(ser.Ok(prefix + "COPY completed", Some(tag)))
        case Some(err) => Seq(ser.No(prefix + "COPY failed: " + err, Some(tag)))
      }
    }
  }
  
  def handleUid(tag: String, command: cli.UidCommand): Future[Seq[ser]] = command match {
    case cli.UidCommand.Copy(cli.Copy(_, set, mailbox)) => handleCopy(tag, set, mailbox, true)
    case cli.UidCommand.Fetch(fetch) => handleFetch(fetch.copy(tag = tag), true)
    case cli.UidCommand.Store(cli.Store(_, set, dataItem)) => handleStore(tag, set, dataItem, true)
    case cli.UidCommand.Search(cli.Search(_, criteria, charset)) => handleSearch(tag, criteria, charset, true)
  }
  
  def messageToFetchItems(msg: MailboxMessage, itemsRequested: Seq[cli.FetchDataItem]): Future[Seq[ser.FetchDataItem]] = {
    // Grab flags first, because if they change we have to send them at the end if not already requested
    // TODO: for now we ignore non-standard flags
    val flags = msg.flags
    // We have to fold because some things (like flags) can change as a result of other things
    val items = itemsRequested.foldLeft(Future.successful(Seq.empty[ser.FetchDataItem])) { case (fut, item) =>
      fut.flatMap { seq =>
        val result = item match {
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
              msg.getText(Seq.empty, None, None).flatMap { text =>
                msg.markSeen().map { _ =>
                  Seq(ser.FetchDataItem.Rfc822(headers.mkString("\r\n") + "\r\n\r\n" + text.getOrElse("")))
                }
              }
            }
          case cli.FetchDataItem.Rfc822Header =>
            msg.getHeaders(Seq.empty, Seq.empty).map { headers =>
              Seq(ser.FetchDataItem.Rfc822Header(headers.mkString("\r\n") + "\r\n"))
            }
          case cli.FetchDataItem.Rfc822Size =>
            Future.successful(Seq(ser.FetchDataItem.Rfc822Size(msg.size)))
          case cli.FetchDataItem.Rfc822Text =>
            msg.getText(Seq.empty, None, None).flatMap { text =>
              msg.markSeen().map(_ => text.map(ser.FetchDataItem.Rfc822Text(_)).toSeq)
            }
          case cli.FetchDataItem.Uid =>
            Future.successful(Seq(ser.FetchDataItem.Uid(msg.uid)))
        }
        // Combine with previous
        result.map(seq ++ _)
      }
    }
    // If flags weren't asked for but they did change, we need to add it to the end
    if (itemsRequested.lastOption == Some(cli.FetchDataItem.Flags)) items
    else items.map { items =>
      if (msg.flags == flags) items
      else items :+ ser.FetchDataItem.Flags(msg.flags.toSeq)
    }
  }
  
  def fetchBodyPart(
    msg: MailboxMessage,
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
        // Have to remove duplicate field names per Dovecot tests
        Future.sequence(fields.distinct.map(msg.getHeader(nums, _))).map(_.flatten).map { lines =>
          // Per Dovecot tests, you still return an empty set here
          val headerLines = headerLinesToStr(lines).getOrElse("") + "\r\n"
          Some(ser.FetchDataItem.Body(part, headerLines, offset))
        }
      case Imap.BodyPart.HeaderFieldsNot(nums, fields) =>
        msg.getHeaders(nums, fields).map { lines =>
          val headerLines = headerLinesToStr(lines).getOrElse("") + "\r\n"
          Some(ser.FetchDataItem.Body(part, headerLines, offset))
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
        println("NO1")
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
      case _ => println("NO2"); ???
    }
  }
  
  def envelopeToImapToken(env: Imap.Envelope): ImapToken.List = {
    import ImapToken._
    @inline
    def addrsOrNil(addrs: Seq[Message.Address]): ImapToken =
      if (addrs.isEmpty) Nil
      else List('(', addrs.flatMap(mailAddressToImapTokens))
    List('(', Seq(
      env.date.map(d => Str(Message.DateTimeFormat.format(d), true)).getOrElse(Nil),
      env.subject.map(Str(_, true)).getOrElse(Nil),
      addrsOrNil(env.from),
      addrsOrNil(if (env.sender.isEmpty) env.from else env.sender),
      addrsOrNil(if (env.replyTo.isEmpty) env.from else env.replyTo),
      addrsOrNil(env.to),
      addrsOrNil(env.cc),
      addrsOrNil(env.bcc),
      if (env.inReplyTo.isEmpty) Nil else Str(env.inReplyTo.mkString(" "), true),
      env.messageId.map(m => Str(m.toString, true)).getOrElse(Nil)
    ))
  }
  
  def mailAddressToImapTokens(addr: Message.Address): Seq[ImapToken] = {
    import ImapToken._
    addr match {
      case Message.Address.Mailbox((mailbox, host), personalName, atDomainList) =>
        val domainListToken =
          if (atDomainList.isEmpty) Nil
          else Str(atDomainList.map('@' + _).mkString(","), true)
        Seq(
          List('(',
            Seq(personalName.map(Str(_, true)).getOrElse(Nil), domainListToken, Str(mailbox, true), Str(host, true))
          )
        )
      case Message.Address.Group(display, mailboxes) =>
        List('(', Seq(Nil, Nil, Str(display, true), Nil)) +:
          mailboxes.flatMap(mailAddressToImapTokens) :+
          List('(', Seq(Nil, Nil, Nil, Nil))
    }
  }
  
  def beginIdle(tag: String): Future[Seq[ser]] = {
    // We require a mailbox to be selected
    val currentMailbox = server.currentMailbox
    if (state != State.Selected || currentMailbox.isEmpty)
      Future.successful(Seq(ser.No("Mailbox must be selected for IDLE", Some(tag))))
    else {
      currentMailbox.get.listen(Some(doCallbackUpdate))
      currentlyRunningIdleTagAndUnsetter = Some(tag -> { () => currentMailbox.get.listen(None) })
      // Send back continuation
      Future.successful(Seq(ser.Continuation(Some("idling"))))
    }
  }
  
  def doCallbackUpdate(update: CallbackUpdate): Unit = update match {
    case CallbackUpdate.Exists(amount) => asyncCallback(Seq(ser.Exists(amount)))
  }
  
  def endIdle(failOnNotRunning: Boolean = true): Future[Seq[ser]] = currentlyRunningIdleTagAndUnsetter match {
    case Some((tag, unsetter)) =>
      unsetter()
      currentlyRunningIdleTagAndUnsetter = None
      Future.successful(Seq(ser.Ok("IDLE terminated", Some(tag))))
    case None if failOnNotRunning => failAndClose("Unexpected IDLE")
    case None => Future.successful(Seq.empty)
  }
}
object HighLevelServerHandler {
  val MaxOutOfBandBufferSize = 500
  
  object State extends Enumeration {
    val Started = Value
    val NotAuthenticated = Value
    val Authenticated = Value
    val Selected = Value
    val Logout = Value
  }
}