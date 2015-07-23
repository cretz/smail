package smail.imap
package handler

import akka.stream.stage.SyncDirective
import akka.stream.stage.StatefulStage
import akka.stream.stage.Context
import scala.util.Try
import scala.concurrent.Future

class CloseConnectionStage extends StatefulStage[ServerResponse, ServerResponse] {
  override def initial = new State {
    override def onPush(chunk: ServerResponse, ctx: Context[ServerResponse]): SyncDirective = {
      handleWithFailure(Some(chunk), ctx)
    }
    
    override def onPull(ctx: Context[ServerResponse]): SyncDirective = {
      handleWithFailure(None, ctx)
    }
    
    def handleWithFailure(chunk: Option[ServerResponse], ctx: Context[ServerResponse]): SyncDirective = {
      chunk match {
        case Some(ServerResponse.CloseConnection) => emitAndFinish(chunk.toIterator, ctx)
        case _ => emit(chunk.toIterator, ctx)
      }
    }
  }
}