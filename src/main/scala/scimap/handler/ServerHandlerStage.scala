package scimap
package handler

import akka.stream.stage.SyncDirective
import akka.stream.stage.StatefulStage
import akka.stream.stage.Context
import scala.util.Try
import scala.concurrent.Future
import akka.stream.stage.PushPullStage
import akka.stream.stage.AsyncStage
import akka.stream.stage.AsyncContext
import akka.stream.stage.UpstreamDirective

class ServerHandlerStage(val handler: ServerHandler)
    extends AsyncStage[ClientCommand.ParseResult, Future[Seq[ServerResponse]], Seq[ServerResponse]] {

  type Ctx = AsyncContext[Future[Seq[ServerResponse]], Seq[ServerResponse]]
  
  var pending = Option.empty[Future[Seq[ServerResponse]]]
  
  override def preStart(ctx: Ctx) = handler.registerOutOfBandCallback(ctx.getAsyncCallback.invoke)
 
  override def onAsyncInput(event: Seq[ServerResponse], ctx: Ctx) = ctx.push(Future.successful(event))
  
  override def onPush(elem: ClientCommand.ParseResult, ctx: Ctx) = handler.handle(Some(elem)) match {
    case Some(res) if ctx.isHoldingDownstream =>
      ctx.pushAndPull(res)
    case v =>
      pending = v
      ctx.holdUpstream()
  }
  
  override def onPull(ctx: Ctx) = {
    if (pending.isEmpty) pending = handler.handle(None)
    pending match {
      case Some(res) if ctx.isHoldingUpstream =>
        pending = None
        ctx.pushAndPull(res)
      case Some(res) =>
        pending = None
        ctx.push(res)
      case None =>
        ctx.holdDownstream()
    }
  }
}