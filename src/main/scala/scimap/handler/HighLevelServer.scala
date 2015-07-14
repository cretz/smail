package scimap
package handler

import java.time.ZonedDateTime
import scala.concurrent.Future

trait HighLevelServer {
  import HighLevelServer._
  
  def currentMailbox: Option[Mailbox]
  
  def listen(f: Option[CallbackUpdate => Unit]): Unit
  
  def capabilities(): Future[Seq[Imap.Capability]] = Future.successful(Seq(
    Imap.Capability.Imap4Rev1,
    Imap.Capability.StartTls,
    Imap.Capability.AuthPlain
  ))
  
  def authenticatePlain(username: String, password: String): Future[Boolean]
  def select(mailbox: String, readOnly: Boolean): Future[Option[Mailbox]]
  
  def hierarchyDelimiter: Option[String] = Some("/")
  def hierarchyRoots: Seq[String] = Seq("/", "~")
  def list(tokenSets: Seq[Seq[Imap.ListToken]], startsAtRoot: Boolean): Future[Seq[ListItem]]
  
  def flushCurrentMailboxDeleted(): Future[Unit]
  def closeCurrentMailbox(): Future[Unit]
  def close(): Future[Unit]
}
object HighLevelServer {
  trait Folder {
    def name: String
    def children(): Future[Seq[Folder]]
    def listen(f: Option[CallbackUpdate => Unit]): Unit
  }
  
  trait Mailbox extends Folder {
    def exists: BigInt
    def recent: BigInt
    def firstUnseen: BigInt
    def flags: Set[Imap.Flag]
    def permanentFlags: Set[Imap.Flag]
    def uidValidity: BigInt
    def nextUid: BigInt
    
    def getMessages(start: BigInt, end: BigInt): Future[Option[Seq[Message]]]
  }
  
  trait Message {
    def uid: BigInt
    def bodyStructure: Imap.BodyStructure
    def envelope: Imap.Envelope
    def flags: Set[Imap.Flag]
    def internalDate: ZonedDateTime
    def size: BigInt

    def markSeen(): Future[Unit]
    def getPart(part: Seq[Int]): Future[Option[String]]
    def getHeader(part: Seq[Int], name: String): Future[Seq[String]]
    def getHeaders(part: Seq[Int], notIncluding: Seq[String]): Future[Seq[String]]
    def getMime(part: Seq[Int]): Future[Option[String]]
    def getText(part: Seq[Int], offset: Option[Int], count: Option[Int]): Future[Option[String]]
  }
  
  case class ListItem(path: String, attrs: Seq[Imap.ListAttribute])
  
  sealed trait CallbackUpdate
  object CallbackUpdate {
    case class Exists(count: BigInt) extends CallbackUpdate
  }
}