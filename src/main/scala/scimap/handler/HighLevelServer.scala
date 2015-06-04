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
  def examine(mailbox: String): Option[Mailbox]
  def flushCurrentMailboxDeleted(): Unit
  def closeCurrentMailbox(): Unit
  def close(): Unit
}
object HighLevelServer {
  trait Mailbox {
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
    def getBody(part: Seq[Imap.BodyPart], offset: Option[Int], count: Option[Int]): Option[String]
    def peekBody(part: Seq[Imap.BodyPart], offset: Option[Int], count: Option[Int]): Option[String]
  }
}