package scimap
package handler

import scala.collection.SortedMap

/** This is primarily for testing and to serve as an example of implementing the handler */
class InMemoryServerHandler extends HighLevelServerHandler {
  import HighLevelServerHandler._
  import InMemoryServerHandler._
  
  @volatile
  var users = Map.empty[String, User]
  
  def currentUser = currentUsername.flatMap(users.get)
  
  override def authenticatePlain(username: String, password: String): Boolean = {
    users.get(username).map(_.password) == Some(password)
  }
  
  override def examine(mailbox: String): Option[ExamineMailboxResult] = currentUser.flatMap { user =>
    user.mailboxes.get(mailbox).map { mailbox =>
      ExamineMailboxResult(
        exists = mailbox.messages.size,
        recent = mailbox.messages.count(_._2.flags.contains(Imap.Flag.Recent)),
        firstUnseen = mailbox.messages.find(!_._2.flags.contains(Imap.Flag.Seen)).map(_._1).getOrElse(0),
        flags = mailbox.flags,
        permanentFlags = mailbox.permanentFlags,
        uidValidity = mailbox.uidValidity,
        nextUid = mailbox.messages.lastKey + 1
      )
    }
  }
}

object InMemoryServerHandler {
  class User(
    @volatile var username: String,
    @volatile var password: String,
    @volatile var mailboxes: Map[String, Mailbox]
  )
  
  class Mailbox(
    @volatile var name: String,
    @volatile var messages: SortedMap[BigInt, Message],
    @volatile var flags: Set[Imap.Flag],
    @volatile var permanentFlags: Set[Imap.Flag],
    @volatile var uidValidity: BigInt = System.currentTimeMillis
  )
  
  class Message(
    @volatile var uid: BigInt,
    @volatile var flags: Set[Imap.Flag]
  )
}