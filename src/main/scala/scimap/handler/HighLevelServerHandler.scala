package scimap
package handler

import java.util.Base64
import scala.collection.immutable.NumericRange
import scala.collection.immutable.TreeSet

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
      case (State.NotAuthenticated, Some(cli.CommandSuccess(auth: cli.Authenticate))) =>
        requestAuthentication(auth)
      case (State.NotAuthenticated, Some(cli.UnrecognizedCommand(seq))) if pendingAuthentication.isDefined =>
        handlePendingAuthentication(seq)
      case (State.NotAuthenticated, _) =>
        failAndClose("Not authenticated")
      case (_, Some(cli.CommandSuccess(cli.Capability(tag)))) =>
        handleCapabilities(tag)
      case (State.Authenticated, Some(cli.CommandSuccess(cli.Examine(tag, mailbox)))) =>
        handleExamine(tag, mailbox)
      case (_, Some(cli.CommandSuccess(cli.Noop(tag)))) =>
        handleNoop(tag)
      case (State.Selected, Some(cli.CommandSuccess(fetch: cli.Fetch))) =>
        handleFetch(fetch)
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
      case Seq(ImapToken.Str(userPass)) =>
        // Per RFC-2595, this is a base 64'd string of the null char + UTF-8 user/pass separated by a null char
        val bytes = Base64.getDecoder().decode(userPass)
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
  
  def handleExamine(tag: String, mailbox: String): Seq[ser] = {
    server.examine(mailbox) match {
      case None => Seq(ser.No("Mailbox not found", Some(tag)))
      case Some(result) =>
        state = State.Selected
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
          ser.Ok("EXAMINE completed", Some(tag), Some(ser.StatusResponseCode.ReadOnly))
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
  
  def handleFetch(fetch: cli.Fetch): Seq[ServerResponse] = {
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
        Seq(cli.FetchDataItem.Flags, cli.FetchDataItem.InternalDate,
          cli.FetchDataItem.Rfc822Size, cli.FetchDataItem.Envelope, cli.FetchDataItem.Body(Seq.empty))
      case Right(seq) => seq
    }

    // Convert to fetch responses and add OK at the end
    msgs.map { case (seq, msg) =>
      val fetches: Seq[ser.FetchDataItem] = items.map {
        case cli.FetchDataItem.NonExtensibleBodyStructure =>
          ser.FetchDataItem.NonExtensibleBodyStructure(msg.bodyStructure)
        case cli.FetchDataItem.Body(parts, offset, count) =>
          ser.FetchDataItem.Body(
            parts,
            msg.getBody(parts, offset, count).getOrElse(
              return Seq(ser.Bad("Unable to get requested part", Some(fetch.tag)))
            ),
            offset
          )
        case cli.FetchDataItem.BodyPeek(parts, offset, count) =>
          ser.FetchDataItem.Body(
            parts,
            msg.peekBody(parts, offset, count).getOrElse(
              return Seq(ser.Bad("Unable to get requested part", Some(fetch.tag)))
            ),
            offset
          )
        case cli.FetchDataItem.BodyStructure =>
          ser.FetchDataItem.BodyStructure(msg.bodyStructure)
        case cli.FetchDataItem.Envelope =>
          ser.FetchDataItem.Envelope(msg.envelope)
        case cli.FetchDataItem.Flags =>
          ser.FetchDataItem.Flags(msg.flags.toSeq)
        case cli.FetchDataItem.InternalDate =>
          ser.FetchDataItem.InternalDate(msg.internalDate)
        case cli.FetchDataItem.Rfc822 =>
          ser.FetchDataItem.Rfc822(
            msg.getBody(Seq.empty, None, None).getOrElse(
              return Seq(ser.Bad("Unable to get RFC822 body", Some(fetch.tag)))
            )
          )
        case cli.FetchDataItem.Rfc822Header =>
          ser.FetchDataItem.Rfc822Header(
            msg.getBody(Seq(Imap.BodyPart.Header), None, None).getOrElse(
              return Seq(ser.Bad("Unable to get RFC822 Header body", Some(fetch.tag)))
            )
          )
        case cli.FetchDataItem.Rfc822Size =>
          ser.FetchDataItem.Rfc822Size(msg.size)
        case cli.FetchDataItem.Rfc822Text =>
          ser.FetchDataItem.Rfc822Text(
            msg.getBody(Seq(Imap.BodyPart.Text), None, None).getOrElse(
              return Seq(ser.Bad("Unable to get RFC822 Text body", Some(fetch.tag)))
            )
          )
        case cli.FetchDataItem.Uid =>
          ser.FetchDataItem.Uid(msg.uid)
      }
      ser.Fetch(seq, fetches)
    } :+ ser.Ok("FETCH completed", Some(fetch.tag))
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