package smail.imap
package handler

import java.time.ZonedDateTime
import scala.concurrent.Future

trait HighLevelServer {
  import HighLevelServer._
  
  def currentMailbox: Option[Mailbox]
  
  def listen(f: Option[CallbackUpdate => Unit]): Unit
  
  def capabilities(): Future[Seq[Imap.Capability]] = Future.successful(defaultCapabilities)
  
  def get(mailbox: String): Future[Option[Mailbox]]
  
  def authenticatePlain(username: String, password: String): Future[Boolean]
  def select(mailbox: String, readOnly: Boolean): Future[Option[Mailbox]]
  def create(name: String): Future[Option[String]]
  def delete(name: String): Future[Option[String]]
  def rename(oldName: String, newName: String): Future[Option[String]]
  def subscribe(name: String): Future[Boolean]
  def unsubscribe(name: String): Future[Boolean]
  
  def hierarchyDelimiter: Option[String] = Some("/")
  def hierarchyRoots: Seq[String] = Seq("/", "~")
  def list(tokens: Seq[Imap.ListToken], startsAtRoot: Boolean, subscribedOnly: Boolean): Future[Seq[ListItem]]
  
  def copyMessagesFromCurrent(
    start: BigInt,
    end: Option[BigInt],
    toMailbox: String,
    byUid: Boolean
  ): Future[Option[String]]

  def closeCurrentMailbox(): Future[Unit]
  def close(): Future[Unit]
}
object HighLevelServer {
  val defaultCapabilities = Seq(
    Imap.Capability.Imap4Rev1,
    Imap.Capability.StartTls,
    Imap.Capability.AuthPlain
  )
  
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
    
    def addMessage(message: String, flags: Set[Imap.Flag], date: ZonedDateTime): Future[Option[String]]
    def getMessages(start: BigInt, end: Option[BigInt], byUid: Boolean): Future[Seq[(BigInt, Message)]]
    def checkpoint(): Future[Unit]
    def expunge(): Future[Seq[BigInt]]
    def search(criteria: Seq[Imap.SearchCriterion], returnUids: Boolean): Future[Seq[BigInt]]
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
    def alterFlags(flags: Set[Imap.Flag], operation: Imap.FlagOperation): Future[Unit]
  }
  
  case class ListItem(path: String, attrs: Seq[Imap.ListAttribute])
  
  sealed trait CallbackUpdate
  object CallbackUpdate {
    case class Exists(count: BigInt) extends CallbackUpdate
  }
}