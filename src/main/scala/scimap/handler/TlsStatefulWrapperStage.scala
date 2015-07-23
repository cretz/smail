package scimap
package handler

import akka.stream.stage.{SyncDirective, Context, StageState, StatefulStage}
import akka.util.ByteString

object TlsStatefulWrapperStage {

  type InboundResult = Either[ByteString, ClientCommand.ParseResult]
  case class OutboundResult(response: ServerResponse, secure: Boolean)

  class Inbound(debug: Boolean) extends StatefulStage[ByteString, InboundResult] {
    var secure = false
    val stringToTokenSet = StringToTokenSet()
    val tokenSetToClientCommand = TokenSetToClientCommand()
    override def initial = new State {
      override def onPush(elem: ByteString, ctx: Context[InboundResult]): SyncDirective = {
        // If it's secure, send through untouched
        if (secure) {
          emit(Iterator.single(Left(elem)), ctx)
        } else {
          val str = elem.decodeString("US-ASCII")
          if (debug) println("Client String: " + str)
          val tokenSets = stringToTokenSet(str)
          if (debug) println("Client Tokens: " + tokenSets)
          val results = tokenSets.map(tokenSetToClientCommand)
          if (debug) println("Client Cmd: " + results)
          results.lastOption match {
            case Some(ClientCommand.CommandSuccess(_: ClientCommand.StartTls)) =>
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
            secure = true
            emit(Iterator.empty, ctx)
          case res =>
            emit(Iterator.single(OutboundResult(elem, secure)), ctx)
        }
      }
    }
  }
}
