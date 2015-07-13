package scimap
package handler

import akka.stream.stage.SyncDirective
import akka.stream.stage.StatefulStage
import akka.stream.stage.Context
import scala.util.Try
import scala.concurrent.Future

class ServerHandlerStage(val handler: ServerHandler)
    extends StatefulStage[ClientCommand.ParseResult, Future[Seq[ServerResponse]]] {
  
  override def initial = new State {
    override def onPush(chunk: ClientCommand.ParseResult, ctx: Context[Future[Seq[ServerResponse]]]): SyncDirective = {
      handleWithFailure(Some(chunk), ctx)
    }
    
    override def onPull(ctx: Context[Future[Seq[ServerResponse]]]): SyncDirective = {
      handleWithFailure(None, ctx)
    }
    
    def handleWithFailure(chunk: Option[ClientCommand.ParseResult], ctx: Context[Future[Seq[ServerResponse]]]): SyncDirective = {
      emit(handler.handle(chunk).toIterator, ctx)
    }
  }
}