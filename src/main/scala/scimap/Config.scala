package scimap

import com.typesafe.config.{Config => Conf}
import com.typesafe.config.ConfigFactory

case class Config(
  server: Config.Server
)
object Config {
  def apply(): Config = apply(ConfigFactory.load())
  def apply(conf: Conf): Config = Config(
    server = Server(conf.getConfig("server"))
  )
  
  case class Server(
    daemon: Server.Daemon
  )
  object Server {
    def apply(conf: Conf): Server = Server(
      daemon = Daemon(conf.getConfig("daemon"))
    )
    
    import scala.language.existentials
    case class Daemon(
      interface: String,
      port: Int,
      handlerClass: Class[_ <: ServerHandler]
    )
    object Daemon {
      def apply(conf: Conf): Daemon = Daemon(
        interface = conf.getString("interface"),
        port = conf.getInt("port"),
        handlerClass = getClass.getClassLoader.
          loadClass(conf.getString("interface")).asSubclass(classOf[ServerHandler])
      )
    }
  }
}