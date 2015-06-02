package scimap

import akka.stream.scaladsl.Tcp
import akka.actor.ActorSystem
import akka.stream.FlowMaterializer
import scimap.handler.ServerHandler
import scala.util.Try

case class ServerDaemon(
  interface: String,
  port: Int,
  handler: () => ServerHandler,
  debug: Boolean
)(implicit system: ActorSystem, mat: FlowMaterializer) {
  val future = Tcp().bind(interface, port).runForeach(_.handleWith(ServerHandler.flow(handler, debug)))
  
  import system.dispatcher
  future.onComplete { println("DONE!!", _) }
}
object ServerDaemon {
  def apply(conf: Config.Server.Daemon, debug: Boolean = false)
    (implicit system: ActorSystem, mat: FlowMaterializer): ServerDaemon =
      ServerDaemon(
        interface = conf.interface,
        port = conf.port,
        handler = () => conf.handlerClass.newInstance(),
        debug = debug
      )
}