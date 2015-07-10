package scimap
package handler

import akka.stream.scaladsl.Flow
import akka.util.ByteString
import javax.net.ssl.SSLContext
import akka.stream.scaladsl.BidiFlow
import akka.stream.io.SslTlsInbound
import akka.stream.io.SslTlsOutbound
import akka.stream.io.SessionBytes
import akka.stream.io.SessionTruncated
import akka.stream.scaladsl.FlattenStrategy
import akka.stream.stage.StatefulStage
import akka.stream.stage.Context
import akka.stream.stage.SyncDirective
import akka.stream.io.NegotiateNewSession
import scala.collection.immutable
import akka.stream.io.SendBytes
import akka.stream.BidiShape
import akka.stream.io.SslTls
import akka.stream.io.Server
import akka.stream.io.SslTlsPlacebo
import scala.collection.immutable
import akka.stream.scaladsl.Source

case class FlowBuilder(
  debug: Boolean,
  sslContext: Option[SSLContext] = None,
  cipherSuites: Option[Seq[String]] = None
) {
  
  implicit class RichFlow[T, U](val flow: Flow[T, U, Unit]) {
    def withDebug(prefix: String): Flow[T, U, Unit] = {
      if (!debug) flow
      else flow.map({ v => println(prefix + ": " + v); v })
    }
  }
  
  val negotiation = NegotiateNewSession.copy(enabledCipherSuites = cipherSuites.map(_.to[immutable.Seq]))
  
  def byteStringToClientParseResult(): Flow[ByteString, ClientCommand.ParseResult, Unit] =
    Flow[ByteString].
      map(_.decodeString("US-ASCII")).withDebug("Client String").
      mapConcat(StringToTokenSet()(_).toStream).withDebug("Client Tokens").
      map(TokenSetToClientCommand()).withDebug("Client Cmd")
      
  def serverResponseToByteString(): Flow[ServerResponse, ByteString, Unit] =
    Flow[ServerResponse].
      map(ServerResponseToString).
      withDebug("Server Cmd").
      map(s => ByteString(s + "\r\n", "US-ASCII"))
      
  
  def tlsInboundToClientParseResult(): Flow[SslTlsInbound, ClientCommand.ParseResult, Unit] =
    Flow[SslTlsInbound].map({
      case SessionBytes(_, bytes) => bytes
      case SessionTruncated => sys.error("Truncated")
    }).via(byteStringToClientParseResult())
 
  def serverResponseToTlsOutbound(): Flow[ServerResponse, SslTlsOutbound, Unit] = Flow[ServerResponse].transform {() =>
    new StatefulStage[ServerResponse, SslTlsOutbound] {
      def serverResponseToStringDebug(resp: ServerResponse): String = {
        val str = ServerResponseToString(resp)
        println(s"Server String: $str")
        str
      }
      override def initial = new State {
        override def onPush(chunk: ServerResponse, ctx: Context[SslTlsOutbound]): SyncDirective = chunk match {
          case ServerResponse.CloseConnection =>
            ctx.finish()
          case ServerResponse.StartTls if sslContext.isDefined =>
            emit(Iterator.single(negotiation), ctx)
          case _ =>
            emit(Iterator.single(SendBytes(ByteString(serverResponseToStringDebug(chunk) + "\r\n", "US-ASCII"))), ctx)
        }
      }
    }
  }
  
  def clientParseResultToServer(mkHandler: () => ServerHandler, parallelism: Int = 3):
      Flow[ClientCommand.ParseResult, ServerResponse, Unit] = {
    Flow[ClientCommand.ParseResult].
      transform(() => new ServerHandlerStage(mkHandler())).
      mapAsync(parallelism)(identity).
      mapConcat(immutable.Iterable(_:_*)).
      withDebug("Server Cmd").
      transform(() => new CloseConnectionStage)
  }
  
  def byteStringToByteString(handler: () => ServerHandler): Flow[ByteString, ByteString, Unit] = {
//    val switchableTls = BidiFlow() { b =>
//      val in = Flow[ByteString]
//      in ~> inBcast ~> tlsInbound    ~> inMerge ~> handler ~> outBcast
//            inBcast ~> nonTlsInbound ~> inMerge
//      outBcast ~> tlsOutbound    ~> outMerge ~> out
//      outBcast ~> nonTlsOutbound ~> outMerge
//    }
    
    val bidiFlow = BidiFlow() { b =>
      val inbound = b.add(tlsInboundToClientParseResult().withDebug("Inbound"))
      val outbound = b.add(serverResponseToTlsOutbound().withDebug("Outbound"))
      BidiShape(outbound, inbound)
    }
    val tlsHandler = sslContext.map(SslTls(_, negotiation, Server)).getOrElse(SslTlsPlacebo.forScala)
    // TODO: is reversed ok here?
    val a = bidiFlow.atop(tlsHandler).reversed
    bidiFlow.atop(tlsHandler).reversed.join(clientParseResultToServer(handler))
//    byteStringToClientParseResult().
//      via(clientParseResultToServer(handler)).
//      via(serverResponseToByteString())
  }
}