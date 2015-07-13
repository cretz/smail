package scimap
package handler

import akka.stream.stage.{SyncDirective, Context, StageState, StatefulStage}
import akka.util.ByteString

object TlsStatefulWrapperStage {

  type InboundResult = Either[ByteString, ClientCommand.ParseResult]
  case class OutboundResult(response: ServerResponse, secure: Boolean)

  class Inbound extends StatefulStage[ByteString, InboundResult] {
    var secure = false
    val stringToTokenSet = StringToTokenSet()
    val tokenSetToClientCommand = TokenSetToClientCommand()
    override def initial = new State {
      override def onPush(elem: ByteString, ctx: Context[InboundResult]): SyncDirective = {
        // If it's secure, send through untouched
        if (secure) {
          emit(Iterator.single(Left(elem)), ctx)
        } else {
          val results = stringToTokenSet(elem.decodeString("US-ASCII")).map(tokenSetToClientCommand)
          println("CLIENT COMMAND: " + results)
          results.lastOption match {
            case Some(ClientCommand.CommandSuccess(_: ClientCommand.StartTls)) =>
              println("SWITCHING INBOUND TO TLS")
              secure = true
            case _ => ()
          }
          emit(results.map(Right(_)).toIterator, ctx)
        }
      }
    }
  }

  class Outbound extends StatefulStage[ServerResponse, OutboundResult] {
    override def initial = new State {
      var secure = false
      override def onPush(elem: ServerResponse, ctx: Context[OutboundResult]): SyncDirective = {
        elem match {
          case ServerResponse.CloseConnection => emit(Iterator.empty, ctx)
          case ServerResponse.StartTls =>
            println("SWITCHING OUTBOUND TO TLS")
            secure = true
            emit(Iterator.empty, ctx)
          case res =>
            println("SERVER RESPONSE: " + res)
            emit(Iterator.single(OutboundResult(elem, secure)), ctx)
        }
      }
    }
  }
}
