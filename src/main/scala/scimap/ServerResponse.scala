package scimap

import org.joda.time.DateTime

sealed trait ServerResponse

object ServerResponse {  
  sealed trait StatusResponse extends ServerResponse {
    def name: String
    def tag: Option[String]
    def responseCode: Option[StatusResponseCode]
    def text: String
  }
  
  case class Ok(
    text: String,
    tag: Option[String] = None,
    responseCode: Option[StatusResponseCode] = None
  ) extends StatusResponse {
    override def name = "OK"
  }
  case class No(
    text: String,
    tag: Option[String] = None,
    responseCode: Option[StatusResponseCode] = None
  ) extends StatusResponse {
    override def name = "NO"
  }
  case class Bad(
    text: String,
    tag: Option[String] = None,
    responseCode: Option[StatusResponseCode] = None
  ) extends StatusResponse {
    override def name = "BAD"
  }
  case class PreAuth(text: String, responseCode: Option[StatusResponseCode] = None) extends StatusResponse {
    override def name = "PREAUTH"
    val tag = None
  }
  case class Bye(text: String, responseCode: Option[StatusResponseCode] = None) extends StatusResponse {
    override def name = "BYE"
    val tag = None
  }
  
  sealed trait StatusResponseCode
  object StatusResponseCode {
    case class Alert(text: String) extends StatusResponseCode
    case class BadCharset(charsets: Seq[String]) extends StatusResponseCode
    case class Capability(names: Seq[Imap.Capability]) extends StatusResponseCode
    case class Parse(text: String) extends StatusResponseCode
    case class PermanentFlags(flags: Seq[Imap.Flag]) extends StatusResponseCode
    case object ReadOnly extends StatusResponseCode
    case object ReadWrite extends StatusResponseCode
    case object TryCreate extends StatusResponseCode
    case class UidNext(value: BigInt) extends StatusResponseCode
    case class UidValidity(value: BigInt) extends StatusResponseCode
    case class Unseen(value: BigInt) extends StatusResponseCode
  }
  
  sealed trait MailboxStatusResponse extends ServerResponse
  
  case class Capability(names: Seq[Imap.Capability]) extends MailboxStatusResponse
  
  case class List(
    name: String,
    delimiter: Option[Char],
    nameAttributes: Seq[ListNameAttribute]
  ) extends MailboxStatusResponse
  sealed trait ListNameAttribute
  object ListNameAttribute {
    case object NoInferiors extends ListNameAttribute {
      override def toString = "\\Noinferiors"
    }
    case object NoSelect extends ListNameAttribute {
      override def toString = "\\Noselect"
    }
    case object Marked extends ListNameAttribute {
      override def toString = "\\Marked"
    }
    case object Unmarked extends ListNameAttribute {
      override def toString = "\\Unmarked"
    }
  }
  
  case class LSub(
    name: String,
    delimiter: Option[Char],
    nameAttributes: Seq[ListNameAttribute]
  ) extends MailboxStatusResponse
  case class Status(name: String, info: Seq[(Imap.StatusDataItem, BigInt)]) extends MailboxStatusResponse
  case class Search(numbers: Seq[BigInt]) extends MailboxStatusResponse
  case class Flags(flags: Seq[Imap.Flag]) extends MailboxStatusResponse
  
  sealed trait MailboxSizeResponse extends ServerResponse {
    def messageCount: BigInt
  }
  case class Exists(messageCount: BigInt) extends MailboxSizeResponse
  case class Recent(messageCount: BigInt) extends MailboxSizeResponse
  
  sealed trait MessageStatusResponse extends ServerResponse {
    def messageSequence: BigInt
  }
  
  case class Expunge(messageSequence: BigInt) extends MessageStatusResponse
  
  case class Fetch(messageSequence: BigInt, data: Seq[FetchDataItem]) extends MessageStatusResponse
  sealed trait FetchDataItem
  object FetchDataItem {
    // TODO: note, a bit duped here from the client side
    case class NonExtensibleBodyStructure(list: Imap.BodyStructureItem.List) extends FetchDataItem
    case class Body(
      section: Seq[Imap.BodyPart],
      contents: String,
      originOctet: Option[Int] = None
    ) extends FetchDataItem
    
    case class BodyStructure(list: Imap.BodyStructureItem.List) extends FetchDataItem
    
    // TODO: Get more typesafe here
    case class Envelope(list: Imap.BodyStructureItem.List) extends FetchDataItem
    case class Flags(flags: Seq[Imap.Flag]) extends FetchDataItem
    case class InternalDate(date: DateTime) extends FetchDataItem
    case class Rfc822(contents: String) extends FetchDataItem
    case class Rfc822Header(contents: String) extends FetchDataItem
    case class Rfc822Size(size: BigInt) extends FetchDataItem
    case class Rfc822Text(contents: String) extends FetchDataItem
    case class Uid(uid: BigInt) extends FetchDataItem
  }
  
  case class Continuation(text: Option[String] = None) extends ServerResponse
  case object CloseConnection extends ServerResponse
}