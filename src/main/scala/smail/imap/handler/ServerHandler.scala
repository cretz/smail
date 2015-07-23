package smail.imap
package handler

import scala.concurrent.Future

trait ServerHandler {
  def registerOutOfBandCallback(cb: Seq[ServerResponse] => Unit): Unit
  
  def handle(res: Option[ClientCommand.ParseResult]): Option[Future[Seq[ServerResponse]]]
  
  def failAndClose(reason: String): Future[Seq[ServerResponse]] =
    Future.successful(Seq(ServerResponse.Bye(reason), ServerResponse.CloseConnection))
}