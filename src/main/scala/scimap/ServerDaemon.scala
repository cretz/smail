package scimap

import akka.stream.scaladsl.Tcp
import akka.actor.ActorSystem
import akka.stream.FlowMaterializer

case class ServerDaemon(
  interface: String,
  port: Int,
  handler: () => ServerHandler
)(implicit system: ActorSystem, mat: FlowMaterializer) {
  val future = Tcp().bind(interface, port).runForeach(_.handleWith(ServerHandler.flow(handler)))
}
object ServerDaemon {
  def apply(conf: Config.Server.Daemon)(implicit system: ActorSystem, mat: FlowMaterializer): ServerDaemon =
    ServerDaemon(
      interface = conf.interface,
      port = conf.port,
      handler = () => conf.handlerClass.newInstance()
    )
}