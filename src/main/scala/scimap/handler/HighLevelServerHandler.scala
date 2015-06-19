package scimap
package handler

import scala.collection.immutable.NumericRange
import scala.collection.immutable.TreeSet
import java.util.regex.Pattern
import scala.annotation.tailrec

class HighLevelServerHandler(server: HighLevelServer) extends ServerHandler {
  import HighLevelServerHandler._
  import HighLevelServer._
  import scimap.{ServerResponse => ser, ClientCommand => cli}
  
  var state = State.Started
  var pendingAuthentication = Option.empty[cli.Authenticate]
  
  // TODO: lots of cleanup needed here
  override def handle(res: Option[cli.ParseResult]): Seq[ser] = {
    (state, res) match {
      case (State.Started, None) =>
        handleFirstConnect()
      case (State.Started, _) =>
        failAndClose("Client sent first command")
      case (_, None) =>
        // This means that no command was parsed...TODO: handle server-initiated update
        Seq.empty
      case (_, Some(cli.CommandSuccess(cli.Logout(tag)))) =>
        handleLogout(tag)
      case (State.NotAuthenticated, Some(cli.CommandSuccess(auth: cli.Authenticate))) =>
        requestAuthentication(auth)
      case (State.NotAuthenticated, Some(cli.UnrecognizedCommand(seq))) if pendingAuthentication.isDefined =>
        handlePendingAuthentication(seq)
      case (State.NotAuthenticated, Some(cli.CommandSuccess(cli.StartTls(tag)))) =>
        Seq(ser.Ok("Begin TLS negotiation now", Some(tag)), ser.StartTls)
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
        sys.error("Unknown state/command: " + v)
    }
  }
  
  def handleFirstConnect(): Seq[ser] = {
      // Call this before state change so implementers can know if this was in response to a request or not
      val caps = server.capabilities()
      state = State.NotAuthenticated
      Seq(ser.Ok("Service ready", None, Some(ser.StatusResponseCode.Capability(caps))))
  }
  
  def handleLogout(tag: String): Seq[ser] = {
    server.close()
    state = State.Logout
    Seq(ser.Bye("Server logging out"), ser.Ok("LOGOUT completed", Some(tag)), ser.CloseConnection)
  }
  
  def handleCapabilities(tag: String): Seq[ser] = {
    Seq(ser.Capability(server.capabilities()), ser.Ok("Complete", Some(tag)))
  }
  
  def requestAuthentication(auth: cli.Authenticate): Seq[ser] = {
    if (pendingAuthentication.isDefined) failAndClose("Authentication already pending")
    else if (auth.mechanism != "PLAIN")
      Seq(ser.No("Only PLAIN accepted", Some(pendingAuthentication.get.tag)))
    else {
      pendingAuthentication = Some(auth)
      Seq(ser.Continuation())
    }
  }
  
  def handlePendingAuthentication(tokens: Seq[ImapToken]): Seq[ser] = {
    if (!pendingAuthentication.isDefined || pendingAuthentication.get.mechanism != "PLAIN")
      return Seq(ser.No("Only PLAIN accepted"))
    val tag = pendingAuthentication.get.tag
    pendingAuthentication = None
    tokens match {
      case Seq(ImapToken.Str(userPass, _)) =>
        // Per RFC-2595, this is a base 64'd string of the null char + UTF-8 user/pass separated by a null char
        val bytes = Util.base64Decode(userPass)
        if (bytes.headOption != Some(0)) Seq(ser.Bad("Unable to read credentials", Some(tag)))
        else bytes.tail.indexOf(0) match {
          case -1 => Seq(ser.Bad("Unable to read credentials", Some(tag)))
          case idx => bytes.tail.splitAt(idx) match {
            case (usernameBytes, passwordBytes) =>
              val username = new String(usernameBytes, "UTF-8")
              if (server.authenticatePlain(username, new String(passwordBytes.tail, "UTF-8"))) {
                state = State.Authenticated
                Seq(ser.Ok("Login successful", Some(tag)))
              } else Seq(ser.No("Login failed", Some(tag)))
            case arr =>
              Seq(ser.Bad("Unable to read credentials", Some(tag)))
          }
        }
      case _ => Seq(ser.Bad("Unable to read credentials", Some(tag)))
    }
  }
  
  def handleSelectOrExamine(tag: String, mailbox: String, isSelect: Boolean): Seq[ser] = {
    server.select(mailbox, !isSelect) match {
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
  
  def handleNoop(tag: String): Seq[ser] = {
    var responses = Seq.empty[ser]
    server.currentMailbox.foreach { res =>
      responses :+= ser.Exists(res.exists)
      responses :+= ser.Recent(res.recent)
      // TODO: EXPUNGE and FETCH
    }
    responses :+ ser.Ok("NOOP completed", Some(tag))
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
  
  def handleList(tag: String, reference: String, mailbox: String): Seq[ser] = {
    val delim = server.hierarchyDelimiter
    // If the mailbox is empty, all we need is the delimiter
    if (mailbox.isEmpty)
      return Seq(ser.List("", delim, Seq(Imap.ListAttribute.NoSelect)), ser.Ok("LIST Completed", Some(tag)))
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
    server.list(tokenSets, startsAtRoot).
      map(i => ser.List(i.path, delim, i.attrs)) :+ ser.Ok("LIST Completed", Some(tag))
  }
  
  def handleFetch(fetch: cli.Fetch): Seq[ser] = {
    val mailbox = server.currentMailbox.getOrElse(return Seq(ser.Bad("No mailbox selected", Some(fetch.tag))))
    
    // Get proper sequences to fetch
    val ranges = fetch.set.items.map {
      case num: Imap.SequenceNumber => num.valueOption.getOrElse(BigInt(1)) -> num.valueOption
      case Imap.SequenceRange(low, high) => low.valueOption.getOrElse(BigInt(1)) -> high.valueOption
    }
    
    // Go over each range and ask for messages
    // TODO: This needs to be streaming/lazy, not chunked
    val msgs = Util.bigIntGaps(ranges).foldLeft(Seq.empty[(BigInt, Message)]) { case (msgs, range) =>
      val end = range._2.getOrElse(mailbox.exists)
      mailbox.getMessages(range._1, end) match {
        // Any none means error
        case None =>
          return Seq(ser.Bad("Unable to read message", Some(fetch.tag)))
        case Some(newMsgs) if newMsgs.size != (end - range._1 + 1).toInt =>
          return Seq(ser.Bad("Unable to find all messages", Some(fetch.tag)))
        case Some(newMsgs) => msgs ++ (range._1 to end).zip(newMsgs)
      }
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

    // Convert to fetch responses and add OK at the end
    msgs.flatMap { case (seq, msg) =>
      val fetches: Seq[ser.FetchDataItem] = items.flatMap {
        case cli.FetchDataItem.NonExtensibleBodyStructure =>
          Seq(ser.FetchDataItem.NonExtensibleBodyStructure(bodyStructureToImapToken(msg.bodyStructure.sansExtension)))
        case cli.FetchDataItem.Body(part, offset, count) =>
          val res = fetchBodyPart(msg, part, offset, count).toSeq
          msg.markSeen()
          res
        case cli.FetchDataItem.BodyPeek(part, offset, count) =>
          fetchBodyPart(msg, part, offset, count).toSeq
        case cli.FetchDataItem.BodyStructure =>
          Seq(ser.FetchDataItem.BodyStructure(bodyStructureToImapToken(msg.bodyStructure)))
        case cli.FetchDataItem.Envelope =>
          Seq(ser.FetchDataItem.Envelope(envelopeToImapToken(msg.envelope)))
        case cli.FetchDataItem.Flags =>
          Seq(ser.FetchDataItem.Flags(msg.flags.toSeq))
        case cli.FetchDataItem.InternalDate =>
          Seq(ser.FetchDataItem.InternalDate(msg.internalDate))
        case cli.FetchDataItem.Rfc822 =>
          val headers = msg.getHeaders(Seq.empty, Seq.empty)
          val text = msg.getText(Seq.empty, None, None).getOrElse("")
          Seq(ser.FetchDataItem.Rfc822(headers.mkString("\r\n") + s"\r\n\r\n$text"))
        case cli.FetchDataItem.Rfc822Header =>
          Seq(ser.FetchDataItem.Rfc822Header(msg.getHeaders(Seq.empty, Seq.empty).mkString("\r\n") + "\r\n"))
        case cli.FetchDataItem.Rfc822Size =>
          Seq(ser.FetchDataItem.Rfc822Size(msg.size))
        case cli.FetchDataItem.Rfc822Text =>
          msg.getText(Seq.empty, None, None).map(ser.FetchDataItem.Rfc822Text(_)).toSeq
        case cli.FetchDataItem.Uid =>
          Seq(ser.FetchDataItem.Uid(msg.uid))
      }
      if (fetches.isEmpty) Seq.empty else Seq(ser.Fetch(seq, fetches))
    } :+ ser.Ok("FETCH completed", Some(fetch.tag))
  }
  
  def fetchBodyPart(
    msg: HighLevelServer.Message,
    part: Imap.BodyPart,
    offset: Option[Int] = None,
    count: Option[Int] = None
  ): Option[ser.FetchDataItem.Body] = {
    @inline
    def substr(str: String) =
      if (!offset.isDefined) str
      else str.substring(offset.get, Math.min(str.length, count.getOrElse(str.length) - offset.get))
    def headerLinesToStr(lines: Seq[String]) =
      if (lines.isEmpty) None
      else Some(substr(lines.mkString("\r\n")) + "\r\n") // TODO: where to substr this?
    part match {
      case Imap.BodyPart.Part(nums) =>
        msg.getPart(nums).map(s => ser.FetchDataItem.Body(part, substr(s), offset))
      case Imap.BodyPart.Header(nums) =>
        headerLinesToStr(msg.getHeaders(nums, Seq.empty)).map(s => ser.FetchDataItem.Body(part, s, offset))
      case Imap.BodyPart.HeaderFields(nums, fields) =>
        val lines = fields.flatMap(msg.getHeader(nums, _))
        headerLinesToStr(lines).map(s => ser.FetchDataItem.Body(part, s, offset))
      case Imap.BodyPart.HeaderFieldsNot(nums, fields) =>
        headerLinesToStr(msg.getHeaders(nums, fields)).map(s => ser.FetchDataItem.Body(part, s, offset))
      case Imap.BodyPart.Mime(nums) =>
        msg.getMime(nums).map(s => ser.FetchDataItem.Body(part, substr(s), offset))
      case Imap.BodyPart.Text(nums) =>
        msg.getText(nums, offset, count).map(s => ser.FetchDataItem.Body(part, s, offset))
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
  
  def handleClose(tag: String): Seq[ser] = {
    server.flushCurrentMailboxDeleted()
    server.closeCurrentMailbox()
    state = State.Authenticated
    Seq(ser.Ok("CLOSE completed", Some(tag)))
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