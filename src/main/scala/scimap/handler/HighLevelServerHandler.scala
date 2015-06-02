package scimap
package handler

import java.util.Base64

trait HighLevelServerHandler extends ServerHandler {
  import HighLevelServerHandler._
  import ServerResponse._
  
  var state = State.Started
  var pendingAuthentication = Option.empty[ClientCommand.Authenticate]
  var currentUsername = Option.empty[String]
  
  override def handle(res: Option[ClientCommand.ParseResult]): Seq[ServerResponse] = {
    import ClientCommand._
    (state, res) match {
      case (State.Started, None) =>
        handleFirstConnect()
      case (State.Started, _) =>
        failAndClose("Client sent first command")
      case (_, None) =>
        // This means that no command was parsed...TODO: handle server-initiated update
        Seq.empty
      case (State.NotAuthenticated, Some(CommandSuccess(auth: Authenticate))) =>
        requestAuthentication(auth)
      case (State.NotAuthenticated, Some(UnrecognizedCommand(seq))) if pendingAuthentication.isDefined =>
        handlePendingAuthentication(seq)
      case (_, Some(CommandSuccess(Capability(tag)))) =>
        Seq(ServerResponse.Capability(capabilities()), Ok("Complete", Some(tag)))
      case (State.Authenticated, Some(CommandSuccess(Examine(tag, mailbox)))) =>
        examine(mailbox) match {
          case None => Seq(No("Mailbox not found", Some(tag)))
          case Some(result) =>
            Seq(
              Exists(result.exists),
              Recent(result.recent),
              Ok(
                "Message " + result.firstUnseen + " is first unseen", 
                None,
                Some(StatusResponseCode.Unseen(result.firstUnseen))
              ),
              Flags(result.flags.toSeq),
              Ok("Permanent flags", None, Some(StatusResponseCode.PermanentFlags(result.permanentFlags.toSeq))),
              Ok("UIDs valid", None, Some(StatusResponseCode.UidValidity(result.uidValidity))),
              Ok("Predicted next UID", None, Some(StatusResponseCode.UidNext(result.nextUid))),
              Ok("EXAMINE completed", Some(tag), Some(StatusResponseCode.ReadOnly))
            )
        }
      case v =>
        sys.error("Unknown state/command: " + v)
    }
  }
  
  def handleFirstConnect(): Seq[ServerResponse] = {
      // Call this before state change so implementers can know if this was in response to a request or not
      val caps = capabilities()
      state = State.NotAuthenticated
      Seq(Ok("Service ready", None, Some(StatusResponseCode.Capability(caps))))
  }
  
  def capabilities(): Seq[CapabilityName] = Seq(
    CapabilityName.Imap4Rev1,
    CapabilityName.StartTls,
    CapabilityName.AuthPlain
  )
  
  def requestAuthentication(auth: ClientCommand.Authenticate): Seq[ServerResponse] = {
    if (pendingAuthentication.isDefined) failAndClose("Authentication already pending")
    else if (auth.mechanism != "PLAIN")
      Seq(ServerResponse.No("Only PLAIN accepted", Some(pendingAuthentication.get.tag)))
    else {
      pendingAuthentication = Some(auth)
      Seq(ServerResponse.Continuation())
    }
  }
  
  def handlePendingAuthentication(tokens: Seq[ImapToken]): Seq[ServerResponse] = {
    if (!pendingAuthentication.isDefined || pendingAuthentication.get.mechanism != "PLAIN")
      return Seq(ServerResponse.No("Only PLAIN accepted"))
    val tag = pendingAuthentication.get.tag
    pendingAuthentication = None
    tokens match {
      case Seq(ImapToken.Str(userPass)) =>
        // Per RFC-2595, this is a base 64'd string of the null char + UTF-8 user/pass separated by a null char
        val bytes = Base64.getDecoder().decode(userPass)
        if (bytes.headOption != Some(0)) Seq(ServerResponse.Bad("Unable to read credentials", Some(tag)))
        else bytes.tail.indexOf(0) match {
          case -1 => Seq(ServerResponse.Bad("Unable to read credentials", Some(tag)))
          case idx => bytes.tail.splitAt(idx) match {
            case (usernameBytes, passwordBytes) =>
              val username = new String(usernameBytes, "UTF-8")
              if (authenticatePlain(username, new String(passwordBytes.tail, "UTF-8"))) {
                state = State.Authenticated
                currentUsername = Some(username)
                Seq(ServerResponse.Ok("Login successful", Some(tag)))
              } else Seq(ServerResponse.No("Login failed", Some(tag)))
            case arr =>
              Seq(ServerResponse.Bad("Unable to read credentials", Some(tag)))
          }
        }
      case _ => Seq(ServerResponse.Bad("Unable to read credentials", Some(tag)))
    }
  }
  
  def authenticatePlain(username: String, password: String): Boolean
  
  def examine(mailbox: String): Option[ExamineMailboxResult]
}
object HighLevelServerHandler {
  object State extends Enumeration {
    val Started = Value
    val NotAuthenticated = Value
    val Authenticated = Value
    val Selected = Value
    val Logout = Value
  }
  
  case class ExamineMailboxResult(
    exists: BigInt,
    recent: BigInt,
    firstUnseen: BigInt,
    flags: Set[Imap.Flag],
    permanentFlags: Set[Imap.Flag],
    uidValidity: BigInt,
    nextUid: BigInt
  )
}