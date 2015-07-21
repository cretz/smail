package scimap
package handler

import akka.stream.scaladsl._
import akka.util.ByteString
import javax.net.ssl.SSLContext
import akka.stream.io.SslTlsInbound
import akka.stream.io.SslTlsOutbound
import akka.stream.io.SessionBytes
import akka.stream.io.SessionTruncated
import akka.stream.stage.StatefulStage
import akka.stream.stage.Context
import akka.stream.stage.SyncDirective
import akka.stream.io.NegotiateNewSession
import scimap.handler.TlsStatefulWrapperStage.{InboundResult, Inbound}
import scala.collection.immutable
import akka.stream.io.SendBytes
import akka.stream.BidiShape
import akka.stream.io.SslTls
import akka.stream.io.Server
import akka.stream.io.SslTlsPlacebo
import scala.collection.immutable
import akka.stream.io.SessionBytes
import scimap.handler.TlsStatefulWrapperStage.Outbound
import scimap.handler.TlsStatefulWrapperStage.OutboundResult
import akka.stream.FlowShape
import akka.stream.ActorAttributes
import akka.stream.Supervision

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
    
    def funcTransform[V](funcMaker: () => U => V): Flow[T, V, Unit] = {
      val mkStage = ()  => new StatefulStage[U, V] {
        override def initial = new State {
          val func = funcMaker()
          override def onPush(elem: U, ctx: Context[V]): SyncDirective = {
            emit(Iterator.single(func(elem)), ctx)
          }
        }
      }
      flow.transform(mkStage)
    }
    
    def funcTransformIter[V](funcMaker: () => U => Iterable[V]): Flow[T, V, Unit] = {
      val mkStage = ()  => new StatefulStage[U, V] {
        override def initial = new State {
          val func = funcMaker()
          override def onPush(elem: U, ctx: Context[V]): SyncDirective = {
            emit(func(elem).toIterator, ctx)
          }
        }
      }
      flow.transform(mkStage)
    }
  }
  
  val negotiation = NegotiateNewSession.copy(enabledCipherSuites = cipherSuites.map(_.to[immutable.Seq]))
  
  def byteStringToClientParseResult(): Flow[ByteString, ClientCommand.ParseResult, Unit] =
    Flow[ByteString].
      map(_.decodeString("US-ASCII")).withDebug("Client String").
      funcTransformIter(StringToTokenSet.apply).withDebug("Client Tokens").
      funcTransform(TokenSetToClientCommand.apply).withDebug("Client Cmd")
      
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
        if (debug) println(s"Server String: $str")
        str
      }
      override def initial = new State {
        override def onPush(chunk: ServerResponse, ctx: Context[SslTlsOutbound]): SyncDirective = chunk match {
          case ServerResponse.CloseConnection =>
            ctx.finish()
          case ServerResponse.StartTls if sslContext.isDefined =>
            // Ignore
            emit(Iterator.empty, ctx)
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
      mapConcat(immutable.Iterable(_:_*)).withDebug("Server Cmd").
      transform(() => new CloseConnectionStage)
  }

  def tlsEnabledByteStringToByteString(handler: () => ServerHandler): Flow[ByteString, ByteString, Unit] = {
    val tlsHandler = sslContext.map(SslTls(_, negotiation, Server)).getOrElse(SslTlsPlacebo.forScala)
      
    val switchableTls = FlowGraph.partial() { implicit b =>
      import FlowGraph.Implicits._
      
      val inBcast = b.add(Broadcast[InboundResult](2))
      val insecureInbound = b.add(Flow[InboundResult].mapConcat {
        case Left(_) => immutable.Iterable.empty[ClientCommand.ParseResult]
        case Right(res) => immutable.Iterable(res)
      })
      val tls = b.add(tlsHandler)
      val secureInbound = b.add(Flow[InboundResult].mapConcat {
        case Right(_) => immutable.Iterable.empty[ByteString]
        case Left(res) => immutable.Iterable(res)
      })
      val inMerge = b.add(Merge[ClientCommand.ParseResult](2))
      val inHandler = b.add(clientParseResultToServer(handler).transform(() => new Outbound))
      val outBcast = b.add(Broadcast[OutboundResult](2))
      val insecureOutbound = b.add(Flow[OutboundResult].mapConcat {
        case OutboundResult(res, true) => immutable.Iterable.empty[ServerResponse]
        case OutboundResult(res, false) => immutable.Iterable(res)
      })
      val secureOutbound = b.add(Flow[OutboundResult].mapConcat {
        case OutboundResult(res, false) => immutable.Iterable.empty[ServerResponse]
        case OutboundResult(res, true) => immutable.Iterable(res)
      })
      val outMerge = b.add(Merge[ByteString](2))
      
      inMerge <~ insecureInbound <~ inBcast
      tls.in2 <~ secureInbound <~ inBcast
      inMerge <~ b.add(tlsInboundToClientParseResult) <~ tls.out2
      outBcast <~ inHandler <~ inMerge
      
      outMerge <~ b.add(serverResponseToByteString) <~ insecureOutbound <~ outBcast
      tls.in1 <~ b.add(serverResponseToTlsOutbound) <~ secureOutbound <~ outBcast
      outMerge <~ tls.out1
      
      FlowShape(inBcast.in, outMerge.out)
    }
    Flow[ByteString].transform(() => new Inbound).via(switchableTls)
  }
}