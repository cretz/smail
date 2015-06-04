package scimap

import java.time.format.DateTimeFormatter
import java.time.ZonedDateTime

object Imap {
  //DQUOTE date-day-fixed "-" date-month "-" date-year SP time SP zone DQUOTE
  lazy val dateTimeFormat = DateTimeFormatter.ofPattern("dd-MMM-yyyy HH:mm:ss Z")
  //date-day "-" date-month "-" date-year
  lazy val dateFormat = DateTimeFormatter.ofPattern("dd-MMM-yyyy")
  //RFC2822 - 3.3
  lazy val mailDateTimeFormat = DateTimeFormatter.ofPattern("dd MMM yyyy HH:mm:ss Z")
  
  case class SequenceSet(items: Seq[SequenceSetItem])
  sealed trait SequenceSetItem
  case class SequenceRange(low: SequenceNumber, high: SequenceNumber) extends SequenceSetItem
  sealed trait SequenceNumber extends SequenceSetItem {
    def valueOption: Option[BigInt]
  }
  case class SequenceNumberLiteral(value: BigInt) extends SequenceNumber {
    def valueOption = Some(value)
  }
  case object SequenceNumberAll extends SequenceNumber {
    def valueOption = None
  }
  
  sealed trait Flag
  object Flag {
    case object Answered extends Flag {
      override def toString = "\\Answered"
    }
    case object Flagged extends Flag {
      override def toString = "\\Flagged"
    }
    case object Deleted extends Flag {
      override def toString = "\\Deleted"
    }
    case object Seen extends Flag {
      override def toString = "\\Seen"
    }
    case object Draft extends Flag {
      override def toString = "\\Draft"
    }
    case object Recent extends Flag {
      override def toString = "\\Recent"
    }
    case class Keyword(keyword: String) extends Flag {
      override def toString = keyword
    }
    case class Extension(extension: String) extends Flag {
      override def toString = "\\" + extension
    }
    case object AcceptNewKeyword extends Flag {
      override def toString = "\\*"
    }
  }
  
  sealed trait StatusDataItem
  object StatusDataItem {
    case object Messages extends StatusDataItem {
      override def toString = "MESSAGES"
    }
    case object Recent extends StatusDataItem {
      override def toString = "RECENT"
    }
    case object UidNext extends StatusDataItem {
      override def toString = "UIDNEXT"
    }
    case object UidValidity extends StatusDataItem {
      override def toString = "UIDVALIDITY"
    }
    case object Unseen extends StatusDataItem {
      override def toString = "UNSEEN"
    }
  }
  
  sealed trait Capability
  object Capability {
    case object Imap4Rev1 extends Capability
    case object StartTls extends Capability
    case object LoginDisabled extends Capability
    case class Auth(mechanism: String) extends Capability
    val AuthPlain = Auth("PLAIN")
    case class Custom(contents: String, prefixWithX: Boolean = true) extends Capability
  }
  
  // TODO: Make the body structure stuff more type safe such as special TEXT versions
  //  and more explicit requirements of which values need to be present
  sealed trait BodyStructure {
    def sansExtension: BodyStructure
  }
  
  case class BodyStructureMulti(
    parts: Seq[BodyStructure],
    subType: String,
    extension: Option[BodyStructureMultiExtension] = None
  ) extends BodyStructure {
    override def sansExtension: BodyStructure = copy(
      parts = parts.map(_.sansExtension),
      extension = None
    )
  }
  
  case class BodyStructureMultiExtension(
    parameters: Map[String, String] = Map.empty,
    disposition: Option[(String, Map[String, String])] = None,
    language: Seq[String] = Seq.empty,
    location: Seq[String] = Seq.empty
  )
  
  case class BodyStructureSingle(
    bodyType: String,
    subType: String,
    parameters: Map[String, String] = Map.empty,
    id: Option[String] = None,
    description: Option[String] = None,
    encoding: Option[String] = None,
    size: Int,
    lineCount: Option[Int] = None,
    extension: Option[BodyStructureSingleExtension] = None
  ) extends BodyStructure {
    override def sansExtension: BodyStructure = copy(extension = None)
  }
  
  case class BodyStructureSingleExtension(
    md5: Option[String] = None,
    disposition: Option[(String, Map[String,String])] = None,
    language: Seq[String] = Seq.empty,
    location: Seq[String] = Seq.empty
  )

  sealed trait BodyPart
  object BodyPart {
    case class Number(num: Int) extends BodyPart
    case object Header extends BodyPart
    case class HeaderFields(fields: Seq[String]) extends BodyPart
    case class HeaderFieldsNot(fields: Seq[String]) extends BodyPart
    case object Mime extends BodyPart
    case object Text extends BodyPart
  }
  
  case class Envelope(
    date: Option[ZonedDateTime] = None,
    subject: Option[String] = None,
    from: Seq[MailAddress] = Seq.empty,
    sender: Seq[MailAddress] = Seq.empty,
    replyTo: Seq[MailAddress] = Seq.empty,
    to: Seq[MailAddress] = Seq.empty,
    cc: Seq[MailAddress] = Seq.empty,
    bcc: Seq[MailAddress] = Seq.empty,
    inReplyTo: Option[(String, String)] = None,
    messageId: Option[(String, String)] = None
  )
  
  sealed trait MailAddress {
    override def toString: String = ???
  }
  case class MailboxAddress(
    mailbox: (String, String),
    displayName: Option[String] = None
  ) extends MailAddress
  case class GroupAddress(
    displayName: String,
    mailboxes: Seq[MailboxAddress]
  ) extends MailAddress
}