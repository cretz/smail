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
  
  def tlsInboundToClientParseResult(): Flow[SslTlsInbound, ClientCommand.ParseResult, Unit] =
    Flow[SslTlsInbound].map({
      case SessionBytes(_, bytes) => bytes
      case SessionTruncated => sys.error("Truncated")
    }).via(byteStringToClientParseResult())
 
  def serverResponseToTlsOutbound(): Flow[ServerResponse, SslTlsOutbound, Unit] = Flow[ServerResponse].transform {() =>
    new StatefulStage[ServerResponse, SslTlsOutbound] {
      val serverResponseToString = ServerResponseToString()
      def serverResponseToStringDebug(resp: ServerResponse): String = {
        val str = serverResponseToString(resp)
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
  
  def clientParseResultToServer(handler: () => ServerHandler): Flow[ClientCommand.ParseResult, ServerResponse, Unit] =
    Flow[ClientCommand.ParseResult].transform(() => new ServerHandlerStage(handler())).withDebug("Server Cmd")
  
  def byteStringToByteString(handler: () => ServerHandler): Flow[ByteString, ByteString, Unit] = {
    val bidiFlow = BidiFlow() { b =>
      val outbound = b.add(serverResponseToTlsOutbound().withDebug("Outbound"))
      val inbound = b.add(tlsInboundToClientParseResult().withDebug("Inbound"))
      BidiShape(outbound, inbound)
    }
    val tlsHandler = sslContext.map(SslTls(_, negotiation, Server)).getOrElse(SslTlsPlacebo.forScala)
    // TODO: is reversed ok here?
    bidiFlow.atop(tlsHandler).reversed.join(clientParseResultToServer(handler))
  }
}