package scimap

import akka.stream.ActorFlowMaterializer
import akka.actor.ActorSystem

object Main extends App {
  implicit val system = ActorSystem("scimap-server")
  implicit val materializer = ActorFlowMaterializer()

  ServerDaemon(Config().server.daemon)
}