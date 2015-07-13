package scimap

import org.specs2.specification.AroundEach
import org.specs2.execute.AsResult
import org.specs2.execute.Result
import org.specs2.specification.ForEach
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.ActorMaterializer
import javax.mail.Session
import java.util.Properties
import akka.stream.ActorMaterializerSettings
import scimap.handler.InMemoryServer
import scimap.handler.HighLevelServerHandler
import javax.net.ssl.SSLContext
import java.security.KeyStore
import javax.net.ssl.KeyManagerFactory
import java.security.SecureRandom
import javax.net.ssl.TrustManagerFactory
import javax.net.ssl.X509TrustManager
import java.security.cert.X509Certificate

trait JavaMailMemoryServer extends ForEach[JavaMailMemoryServer.Context] {
  override def foreach[R: AsResult](f: JavaMailMemoryServer.Context => R): Result = {
    val server = new InMemoryServer()
    val ctx = new JavaMailMemoryServer.Context()
    try AsResult(f(ctx))
    finally {
      if (ctx.storeInitialized) ctx.store.close()
      ctx.system.shutdown()
      ctx.system.awaitTermination()
    }
  }
}
object JavaMailMemoryServer {  
  class Context(val server: InMemoryServer = new InMemoryServer()) {
    implicit val system = ActorSystem("scimap-server")
    implicit val materializer = ActorMaterializer(
      ActorMaterializerSettings(system).withDebugLogging(true)
    )
    import system.dispatcher
    
    var daemon = ServerDaemon("127.0.0.1", 143, () => new HighLevelServerHandler(server), true, None, None)
    
    var username = "foo"
    var password = "bar"
    var useClientTls = false
    
    def useTls(
      password: String = "changeme",
      keyStoreResourcePath: String = "/keystore",
      trustStoreResourcePath: String = "/truststore",
      cipherSuites: Option[Seq[String]] =
        // Insecure on purpose so we don't have to have JSSE installed
        Some(Seq("TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA", "TLS_RSA_WITH_AES_128_CBC_SHA"))
    ): Unit = {
      useClientTls = true
      daemon = daemon.copy(
        sslContext = Some(initSslContext(password, keyStoreResourcePath, trustStoreResourcePath)),
        cipherSuites = cipherSuites
      )
    }
    
    lazy val session = {
      val props = new Properties()
      if (useClientTls) {
        props.setProperty("mail.imap.starttls.required", "true")
        props.setProperty("mail.imap.ssl.ciphersuites", daemon.cipherSuites.get.mkString(" "))
        props.setProperty("mail.imap.ssl.trust", "*")
      }
      val session = Session.getDefaultInstance(props)
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
 
  def initSslContext(
    password: String,
    keyStoreResourcePath: String,
    trustStoreResourcePath: String
  ): SSLContext = {
    // Most of this taken from akka stream UT
    val keyStore = KeyStore.getInstance(KeyStore.getDefaultType)
    keyStore.load(getClass.getResourceAsStream(keyStoreResourcePath), password.toCharArray)
    val keyManagerFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    keyManagerFactory.init(keyStore, password.toCharArray)
    
    val trustStore = KeyStore.getInstance(KeyStore.getDefaultType)
    trustStore.load(getClass.getResourceAsStream(trustStoreResourcePath), password.toCharArray)
    val trustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    trustManagerFactory.init(trustStore)
    
    val ctx = SSLContext.getInstance("TLS")
    ctx.init(keyManagerFactory.getKeyManagers, trustManagerFactory.getTrustManagers, new SecureRandom)
    ctx
  }
}