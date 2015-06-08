package scimap

import akka.stream.scaladsl.Tcp
import akka.actor.ActorSystem
import akka.stream.FlowMaterializer
import scimap.handler.ServerHandler
import scimap.handler.FlowBuilder
import scala.util.Try
import scimap.handler.HighLevelServerHandler
import javax.net.ssl.SSLContext
import scala.concurrent.Future

case class ServerDaemon(
  interface: String,
  port: Int,
  handler: () => ServerHandler,
  debug: Boolean,
  sslContext: Option[SSLContext],
  cipherSuites: Option[Seq[String]]
)(implicit system: ActorSystem, mat: FlowMaterializer) {
  
  def start(): Future[Unit] = Tcp().bind(interface, port).runForeach(_.handleWith(
    FlowBuilder(debug, sslContext, cipherSuites).byteStringToByteString(handler)))
}
object ServerDaemon {
  def apply(conf: Config.Server.Daemon, debug: Boolean = false)
    (implicit system: ActorSystem, mat: FlowMaterializer): ServerDaemon =
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