package scimap
package handler

import java.time.ZonedDateTime

trait HighLevelServer {
  import HighLevelServer._
  
  def currentMailbox: Option[Mailbox]
  
  def capabilities(): Seq[Imap.Capability] = Seq(
    Imap.Capability.Imap4Rev1,
    Imap.Capability.StartTls,
    Imap.Capability.AuthPlain
  )
  
  def authenticatePlain(username: String, password: String): Boolean
  def select(mailbox: String, readOnly: Boolean): Option[Mailbox]
  
  def hierarchyDelimiter: Option[String] = Some("/")
  def hierarchyRoots: Seq[String] = Seq("/", "~")
  def list(tokenSets: Seq[Seq[Imap.ListToken]], startsAtRoot: Boolean): Seq[ListItem]
  
  def flushCurrentMailboxDeleted(): Unit
  def closeCurrentMailbox(): Unit
  def close(): Unit
}
object HighLevelServer {
  trait Folder {
    def name: String
    def children: Seq[Folder]
  }
  
  trait Mailbox extends Folder {
    def exists: BigInt
    def recent: BigInt
    def firstUnseen: BigInt
    def flags: Set[Imap.Flag]
    def permanentFlags: Set[Imap.Flag]
    def uidValidity: BigInt
    def nextUid: BigInt
    
    def getMessages(start: BigInt, end: BigInt): Option[Seq[Message]]
  }
  
  trait Message {
    def uid: BigInt
    def bodyStructure: Imap.BodyStructure
    def envelope: Imap.Envelope
    def flags: Set[Imap.Flag]
    def internalDate: ZonedDateTime
    def size: BigInt

    def markSeen(): Unit
    def getPart(part: Seq[Int]): Option[String]
    def getHeader(part: Seq[Int], name: String): Seq[String]
    def getHeaders(part: Seq[Int], notIncluding: Seq[String]): Seq[String]
    def getMime(part: Seq[Int]): Option[String]
    def getText(part: Seq[Int], offset: Option[Int], count: Option[Int]): Option[String]
  }
  
  case class ListItem(path: String, attrs: Seq[Imap.ListAttribute])
}