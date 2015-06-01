package scimap

import akka.stream.stage.SyncDirective
import akka.stream.stage.StatefulStage
import akka.stream.stage.Context
import akka.util.ByteString
import akka.stream.scaladsl.Flow

sealed trait ServerHandler extends StatefulStage[ClientCommand.ParseResult, ServerResponse] {
  override def initial = new State {
      override def onPush(chunk: ClientCommand.ParseResult, ctx: Context[ServerResponse]): SyncDirective = {
        emit(handle(chunk).iterator, ctx)
      }
    }

  def handle(commandParseResult: ClientCommand.ParseResult): Seq[ServerResponse]
}

object ServerHandler {
  def flow(handler: () => ServerHandler) =
    Flow[ByteString].
      map(_.decodeString("US-ASCII")).
      transform(() => new StringToTokenSet.Stage()).
      transform(() => new TokenSetToClientCommand.Stage()).
      transform(handler).
      transform(() => new ServerResponseToString.Stage()).
      map(ByteString(_, "US-ASCII"))
  
  trait HighLevel extends ServerHandler {
    
  }
}