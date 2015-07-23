package smail.imap

import smail.imap.handler._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializerSettings
import akka.stream.ActorMaterializer
import scala.concurrent.Await
import scala.concurrent.duration._

// For use w/ the dovecot ImapTest
object ImapTestServer extends App {
  import InMemoryServer._
  
  println("Starting IMAP server on 0.0.0.0:143")
  
  implicit val system = ActorSystem("smail-imap-server")
  implicit val materializer = ActorMaterializer(
    ActorMaterializerSettings(system).withDebugLogging(true)
  )
  import system.dispatcher
    
  // Create server with user
  val server = new InMemoryServer()
  server.users += "foo" -> new InMemoryUser(
    username = "foo",
    password = "bar",
    Map(
      "INBOX" -> new InMemoryMailbox(
        "INBOX",
        flags = Set(Imap.Flag.Answered),
        permanentFlags = Set.empty
      )
    )
  )
  
  // Start
  val daemon = ServerDaemon("0.0.0.0", 143, () => new HighLevelServerHandler(server), true, None, None)
  val daemonStartInfo = daemon.start()
  Await.ready(daemonStartInfo.bindFuture, 10.seconds)
  
  println("Started and waiting for connection...")
  system.awaitTermination()
}