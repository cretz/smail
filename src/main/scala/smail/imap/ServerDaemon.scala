package smail.imap

import akka.stream.scaladsl.Tcp
import akka.actor.ActorSystem
import akka.stream.Materializer
import smail.imap.handler.ServerHandler
import smail.imap.handler.FlowBuilder
import scala.util.Try
import smail.imap.handler.HighLevelServerHandler
import javax.net.ssl.SSLContext
import scala.concurrent.Future
import scala.concurrent.Promise

case class ServerDaemon(
  interface: String,
  port: Int,
  handler: () => ServerHandler,
  debug: Boolean,
  sslContext: Option[SSLContext],
  cipherSuites: Option[Seq[String]]
)(implicit system: ActorSystem, mat: Materializer) {
  lazy val flow = FlowBuilder(debug, sslContext, cipherSuites).tlsEnabledByteStringToByteString(handler)
  def start(): ServerDaemon.StartResult = {
    val boundPromise = Promise[Tcp.ServerBinding]()
    val unboundFuture = Tcp().bind(interface, port).
      mapMaterializedValue(boundPromise.completeWith).
      runForeach(_.handleWith(flow))
    ServerDaemon.StartResult(boundPromise.future, unboundFuture)
  }
}
object ServerDaemon {
  case class StartResult(bindFuture: Future[Tcp.ServerBinding], unboundFuture: Future[Unit])
  
  def apply(conf: Config.Server.Daemon, debug: Boolean = false)
      (implicit system: ActorSystem, mat: Materializer): ServerDaemon = {
    import system.dispatcher
    ServerDaemon(
      interface = conf.interface,
      port = conf.port,
      handler = conf.handlerClass match {
        case Left(handler) => () => handler.newInstance()
        case Right(server) => () => new HighLevelServerHandler(server.newInstance())
      },
      debug = debug,
      // TODO: SSL config
      sslContext = None,
      cipherSuites = None
    )
  }
}