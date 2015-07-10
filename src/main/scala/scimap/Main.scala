package scimap

import akka.stream.ActorMaterializer
import akka.actor.ActorSystem

object Main extends App {
  implicit val system = ActorSystem("scimap-server")
  implicit val materializer = ActorMaterializer()

  val config = Config()
  ServerDaemon(config.server.daemon, config.debug)
}