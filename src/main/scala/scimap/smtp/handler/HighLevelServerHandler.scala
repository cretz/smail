package scimap.smtp
package handler

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class HighLevelServerHandler(val server: HighLevelServer)
    (implicit val execCtx: ExecutionContext) extends ServerHandler {
  import HighLevelServerHandler._
  import scimap.smtp.{ServerResponse => ser, ClientCommand => cli}
  
  var state: State = State.BeforeHello
  
  def handle(res: Option[cli]): Option[Future[Seq[ser]]] = {
    state -> res match {
      case (State.BeforeHello, None) =>
        Some(handleConnectionOpen())
      case (State.BeforeHello, Some(cli.Hello(domain))) =>
        Some(handleHello(Some(domain), false))
      case (State.BeforeHello, Some(cli.ExtendedHello(address))) =>
        Some(handleHello(address, true))
      case (State.Hello, Some(cli.Mail(mailbox, params))) =>
        Some(handleMail(mailbox, params))
      case (State.InMail, Some(cli.Recipient(path, params))) =>
        Some(handleRecipient(path, params))
      case (State.InMail, Some(cli.Data)) =>
        Some(handleDataBegin())
      case (State.InData, Some(cli.DataLine(line))) =>
        Some(handleDataLine(line))
      case (State.InData, Some(cli.DataEnd)) =>
        Some(handleDataEnd())
      // TODO: more
      case (state, cmd) =>
        sys.error("Unrecognized state " + state + " and " + cmd)
    }
  }
  
  def handleFailure(failure: Smtp.Failure): Seq[ser] = {
    Seq(ser.Normal(failure.code, failure.contents))
  }
  
  def handleConnectionOpen(): Future[Seq[ser]] = {
    server.description().map {
      case Left(description) =>
        state = State.InMail
        Seq(ser.Normal(Smtp.ReplyCode.ServiceReady, description))
      case Right(failure) =>
        handleFailure(failure)
    }
  }
  
  def handleHello(address: Option[String], includeExtensions: Boolean): Future[Seq[ser]] = {
    server.greeting(address).flatMap {
      case Left(greeting) if includeExtensions =>
        state = State.Hello
        server.extensions().map {
          case Left(extensions) if extensions.isEmpty =>
            Seq(ser.Normal(Smtp.ReplyCode.Ok, greeting.toString))
          case Left(extensions) =>
            ser.Normal(Smtp.ReplyCode.Ok, greeting.toString, true) +:
              extensions.dropRight(1).map(ser.Normal(Smtp.ReplyCode.Ok, _, true)) :+
              ser.Normal(Smtp.ReplyCode.Ok, extensions.last)
          case Right(failure) =>
            handleFailure(failure)
        }
      case Left(greeting) =>
        state = State.Hello
        Future.successful(Seq(ser.Normal(Smtp.ReplyCode.Ok, greeting.toString)))
      case Right(failure) =>
        Future.successful(handleFailure(failure))
    }
  }
  
  def handleMail(mailbox: Option[Smtp.Address], params: Seq[(String, String)]): Future[Seq[ser]] = {
    server.beginMail(mailbox, params).map {
      case None => Seq(ser.Normal(Smtp.ReplyCode.Ok))
      case Some(failure) => handleFailure(failure)
    }
  }
  
  def handleRecipient(path: cli.RecipientPath, params: Seq[(String, String)]): Future[Seq[ser]] = {
    val future = path match {
      case cli.RecipientPath.Postmaster =>
        server.addPostmasterRecipient(None, params)
      case cli.RecipientPath.PostmasterDomain(domain) =>
        server.addPostmasterRecipient(Some(domain), params)
      case cli.RecipientPath.Path(address) =>
        server.addUserRecipient(address, params)
    }
    future.map {
      case None =>
        Seq(ser.Normal(Smtp.ReplyCode.Ok))
      case Some(failure) =>
        handleFailure(failure)
    }
  }
  
  def handleDataBegin(): Future[Seq[ser]] = {
    server.beginData().map {
      case None =>
        state = State.InData
        Seq(ser.Normal(Smtp.ReplyCode.StartMailInput))
      case Some(failure) =>
        handleFailure(failure)
    }
  }
  
  def handleDataLine(line: String): Future[Seq[ser]] = {
    server.beginData().map {
      case None =>
        state = State.InData
        Seq(ser.Normal(Smtp.ReplyCode.StartMailInput))
      case Some(failure) =>
        handleFailure(failure)
    }
  }
  
  def handleDataEnd(): Future[Seq[ser]] = {
    server.endData().flatMap {
      case None =>
        server.endMail().map {
          case None =>
            state = State.Hello
            Seq(ser.Normal(Smtp.ReplyCode.Ok))
          case Some(failure) =>
            handleFailure(failure)
        }
      case Some(failure) =>
        Future.successful(handleFailure(failure))
    }
  }
}
object HighLevelServerHandler {
  sealed trait State
  object State {
    case object BeforeHello extends State
    case object Hello extends State
    case object InMail extends State
    case object InData extends State
  }
}