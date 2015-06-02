package scimap
package handler

import akka.stream.stage.SyncDirective
import akka.stream.stage.StatefulStage
import akka.stream.stage.Context
import scala.util.Try

class ServerHandlerStage(handler: ServerHandler) extends StatefulStage[ClientCommand.ParseResult, ServerResponse] {
  override def initial = new State {
    override def onPush(chunk: ClientCommand.ParseResult, ctx: Context[ServerResponse]): SyncDirective = {
      handleWithFailure(Some(chunk), ctx)
    }
    
    override def onPull(ctx: Context[ServerResponse]): SyncDirective = {
      handleWithFailure(None, ctx)
    }
    
    def handleWithFailure(chunk: Option[ClientCommand.ParseResult], ctx: Context[ServerResponse]): SyncDirective = {
      val res = Try(handler.handle(chunk)).recover({ case t => handler.onInternalError(t) }).get
      if (res.lastOption == Some(ServerResponse.CloseConnection)) emitAndFinish(res.iterator, ctx)
      else emit(res.iterator, ctx)
    }
  }
}