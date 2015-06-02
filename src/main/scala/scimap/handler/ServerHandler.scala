package scimap
package handler

import akka.stream.stage.SyncDirective
import akka.stream.stage.StatefulStage
import akka.stream.stage.Context
import akka.util.ByteString
import akka.stream.scaladsl.Flow

trait ServerHandler {
  def handle(res: Option[ClientCommand.ParseResult]): Seq[ServerResponse]
  
  def onInternalError(t: Throwable): Seq[ServerResponse] = {
    println("Failure: ", t, t.getStackTrace().mkString("\n\t", "\n\t", ""))
    failAndClose("Internal error")
  }
  
  def failAndClose(reason: String): Seq[ServerResponse] = {
    Seq(ServerResponse.Bye(reason), ServerResponse.CloseConnection)
  }
}

object ServerHandler {
  def flow(handler: () => ServerHandler, debug: Boolean = false) = {
    implicit class RichFlow[T](val flow: Flow[ByteString, T, Unit]) {
      def withDebug(prefix: String): Flow[ByteString, T, Unit] = {
        if (!debug) flow
        else flow.map({ v => println(prefix + ": " + v); v })
      }
    }
    Flow[ByteString].
      map(_.decodeString("US-ASCII")).withDebug("Client String").
      transform(() => new StringToTokenSet.Stage()).withDebug("Client Tokens").
      transform(() => new TokenSetToClientCommand.Stage()).withDebug("Client Cmd").
      transform(() => new ServerHandlerStage(handler())).withDebug("Server Cmd").
      transform(() => new ServerResponseToString.Stage()).withDebug("Server String").
      map(str => ByteString(str + "\r\n", "US-ASCII"))
  }
}