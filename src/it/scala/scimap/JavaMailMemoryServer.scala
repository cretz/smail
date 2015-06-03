package scimap

import org.specs2.specification.AroundEach
import org.specs2.execute.AsResult
import org.specs2.execute.Result
import org.specs2.specification.ForEach
import akka.actor.ActorSystem
import akka.stream.FlowMaterializer
import akka.stream.ActorFlowMaterializer
import javax.mail.Session
import java.util.Properties
import akka.stream.ActorFlowMaterializerSettings
import scimap.handler.InMemoryServer
import scimap.handler.HighLevelServerHandler

trait JavaMailMemoryServer extends ForEach[JavaMailMemoryServer.Context] {
  override def foreach[R: AsResult](f: JavaMailMemoryServer.Context => R): Result = {
    implicit val system = ActorSystem("scimap-server")
    implicit val materializer = ActorFlowMaterializer(
      ActorFlowMaterializerSettings(system).withDebugLogging(true)
    )
    val server = new InMemoryServer()
    val ctx = new JavaMailMemoryServer.Context(
      ServerDaemon("127.0.0.1", 143, () => new HighLevelServerHandler(server), true),
      server
    )
    try AsResult(f(ctx))
    finally {
      if (ctx.storeInitialized) ctx.store.close()
      system.shutdown()
    }
  }
}
object JavaMailMemoryServer {
  class Context(val daemon: ServerDaemon, val server: InMemoryServer) {
    
    var username = "foo"
    var password = "bar"
    
    lazy val session = {
      val session = Session.getDefaultInstance(new Properties())
      session.setDebug(true)
      session
    }
    var storeInitialized = false
    lazy val store = {
      val store = session.getStore("imap")
      println("Connecting to ", daemon.interface, daemon.port)
      store.connect(daemon.interface, daemon.port, username, password)
      storeInitialized = true
      store
    }
  }
}