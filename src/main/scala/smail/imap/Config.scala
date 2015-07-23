package smail.imap

import com.typesafe.config.{Config => Conf}
import com.typesafe.config.ConfigFactory
import smail.imap.handler.ServerHandler
import smail.imap.handler.HighLevelServer

case class Config(
  debug: Boolean,
  server: Config.Server
)
object Config {
  def apply(): Config = apply(ConfigFactory.load().getConfig("smail"))
  def apply(conf: Conf): Config = Config(
    debug = conf.getBoolean("debug"),
    server = Server(conf.getConfig("imap.server"))
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
      handlerClass: Either[Class[_ <: ServerHandler], Class[_ <: HighLevelServer]]
    )
    object Daemon {
      def apply(conf: Conf): Daemon = Daemon(
        interface = conf.getString("interface"),
        port = conf.getInt("port"),
        handlerClass = {
          val cls = getClass.getClassLoader.loadClass(conf.getString("interface"))
          if (classOf[ServerHandler].isAssignableFrom(cls)) Left(cls.asSubclass(classOf[ServerHandler]))
          else if (classOf[HighLevelServer].isAssignableFrom(cls)) Right(cls.asSubclass(classOf[HighLevelServer]))
          else sys.error("Unrecognized class type: " + cls)
        }
      )
    }
  }
}