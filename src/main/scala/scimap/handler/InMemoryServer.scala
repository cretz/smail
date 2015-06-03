package scimap
package handler

import org.joda.time.DateTime

// Basically only for testing
class InMemoryServer extends HighLevelServer {
  import HighLevelServer._
  import InMemoryServer._
  
  @volatile
  var users = Map.empty[String, InMemoryUser]
  var currentUser = Option.empty[InMemoryUser]
  var currentMailbox = Option.empty[InMemoryMailbox]
  
  def authenticatePlain(username: String, password: String): Boolean =
    users.get(username) match {
      case Some(user) if user.password == password => currentUser = Some(user); true
      case _ => false
    }
  
  def examine(mailbox: String): Option[Mailbox] =
    currentUser.flatMap(_.mailboxes.get(mailbox)).map { ret =>
      currentMailbox = Some(ret)
      ret
    }
}
object InMemoryServer {
  import HighLevelServer._
  
  class InMemoryUser(
    @volatile var username: String,
    @volatile var password: String,
    @volatile var mailboxes: Map[String, InMemoryMailbox]
  )
  
  class InMemoryMailbox(
    @volatile var name: String,
    @volatile var messages: Seq[InMemoryMessage],
    @volatile var flags: Set[Imap.Flag],
    @volatile var permanentFlags: Set[Imap.Flag],
    @volatile var uidValidity: BigInt = System.currentTimeMillis
  ) extends Mailbox {
    def exists = messages.size
    def recent = messages.count(_.flags.contains(Imap.Flag.Recent))
    def firstUnseen = messages.find(!_.flags.contains(Imap.Flag.Seen)).map(_.uid).getOrElse(0)
    def nextUid = messages.foldLeft(BigInt(0)){ case (max, msg) => max.max(msg.uid) } + 1
    
    def getMessages(start: BigInt, end: BigInt): Option[Seq[Message]] = {
      println("Getting from ", start, end)
      val lifted = messages.lift
      val msgs = for (i <- start to end) yield {
        lifted(i.toInt - 1).getOrElse(return None)
      }
      Some(msgs)
    }
  }
  
  class InMemoryMessage(
    @volatile var uid: BigInt,
    @volatile var flags: Set[Imap.Flag],
    @volatile var headers: Map[String, String],
    @volatile var body: String
  ) extends Message {
    def bodyStructure: Imap.BodyStructureItem.List = ???
    def envelope: Imap.BodyStructureItem.List = ???
    def internalDate: DateTime = ???
    def size: BigInt = ???
    
    def getBody(part: Seq[Imap.BodyPart], offset: Option[Int], count: Option[Int]): Option[String] = {
      val result = peekBody(part, offset, count)
      flags += Imap.Flag.Seen
      result
    }
    
    def peekBody(part: Seq[Imap.BodyPart], offset: Option[Int], count: Option[Int]): Option[String] = {
      // TODO: provide a way to give errors here
      val possibleStr = part match {
        case Seq() => peekBody(Seq(Imap.BodyPart.Header), offset.map(_ => 0), count).flatMap(h =>
          peekBody(Seq(Imap.BodyPart.Text), offset.map(_ => 0), count).map(h + "\r\n\r\n" + _)
        )
        case Seq(Imap.BodyPart.Header) => Some(headers.map(h => h._1 + ": " + h._2).mkString("\r\n"))
        case Seq(Imap.BodyPart.Text) => Some(body)
        case _ => None
      }
      val str = possibleStr.getOrElse(return None)
      offset -> count match {
        case (None, _) => Some(str)
        case (Some(offset), None) => None
        case (Some(offset), _) if offset > str.length => None
        case (Some(offset), Some(count)) if count > str.length => Some(str.substring(offset))
        case (Some(offset), Some(count)) => Some(str.substring(offset, count))
      }
    }
  }
}