package scimap

import java.time.format.DateTimeFormatter
import java.time.ZonedDateTime
import java.time.LocalDate

object Imap {
  //DQUOTE date-day-fixed "-" date-month "-" date-year SP time SP zone DQUOTE
  lazy val dateTimeFormat = DateTimeFormatter.ofPattern("dd-MMM-yyyy HH:mm:ss Z")
  //date-day "-" date-month "-" date-year
  lazy val dateFormat = DateTimeFormatter.ofPattern("dd-MMM-yyyy")
  //RFC2822 - 3.3
  lazy val mailDateTimeFormat = DateTimeFormatter.ofPattern("dd MMM yyyy HH:mm:ss Z")
  
  case class SequenceSet(items: Seq[SequenceSetItem]) {
    def contains(value: BigInt): Boolean = items.exists(_.contains(value))
  }
  sealed trait SequenceSetItem {
    def contains(value: BigInt): Boolean
  }
  case class SequenceRange(low: SequenceNumber, high: SequenceNumber) extends SequenceSetItem {
    def contains(value: BigInt): Boolean =
      (!low.isInstanceOf[SequenceNumberLiteral] || low.asInstanceOf[SequenceNumberLiteral].value <= value) &&
        (!high.isInstanceOf[SequenceNumberLiteral] || high.asInstanceOf[SequenceNumberLiteral].value >= value)
  }
  sealed trait SequenceNumber extends SequenceSetItem {
    def valueOption: Option[BigInt]
  }
  case class SequenceNumberLiteral(value: BigInt) extends SequenceNumber {
    def valueOption = Some(value)
    def contains(value: BigInt): Boolean = value == this.value
  }
  case object SequenceNumberAll extends SequenceNumber {
    def valueOption = None
    def contains(value: BigInt): Boolean = true
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
    case object Idle extends Capability
    case class Auth(mechanism: String) extends Capability
    val AuthPlain = Auth("PLAIN")
    case class Custom(contents: String, prefixWithX: Boolean = true) extends Capability
    
    // Extensions...
    case object UidPlus extends Capability
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
    case class Part(nums: Seq[Int]) extends BodyPart
    case class Header(nums: Seq[Int]) extends BodyPart
    case class HeaderFields(nums: Seq[Int], fields: Seq[String]) extends BodyPart 
    case class HeaderFieldsNot(nums: Seq[Int], fields: Seq[String]) extends BodyPart
    case class Mime(nums: Seq[Int]) extends BodyPart
    case class Text(nums: Seq[Int]) extends BodyPart
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
    inReplyTo: Seq[(String, String)] = Seq.empty,
    messageId: Option[(String, String)] = None
  )
  
  sealed trait MailAddress {
    override def toString: String = ???
  }
  object MailAddress {
    def safeString(string: String): String = {
      // Only if the string contains a slash, double quote, or a space do we want to double quote it
      val result = string.replace("\\", "\\\\").replace("\"","\\\"")
      if (result.length > 0 && result.length == string.length && result.indexOf(' ') == -1) result
      else '"' + result + '"'
    }

    def safeDomain(string: String): String = {
      // Only if the string contains a bracket, double quote, or a space do we want to double quote it
      val result = string.replace("\\", "\\\\").replace("[","\\[").replace("]","\\]")
      if (result.length > 0 && result.length == string.length && result.indexOf(' ') == -1) result
      else '[' + result + ']'
    }
    
    def fromString(string: String): Option[MailAddress] = ???
  }

  case class MailboxAddress(
    mailbox: (String, String),
    displayName: Option[String] = None
  ) extends MailAddress {
    override def toString: String = {
      val addr = MailAddress.safeString(mailbox._1) + '@' + MailAddress.safeDomain(mailbox._2)
      displayName.map(MailAddress.safeString).map('[' + _ + "] <" + addr + '>').getOrElse(addr)
    }
  }
  object MailboxAddress {
    def fromString(string: String): Option[MailboxAddress] = ???
  }
  
  case class GroupAddress(
    displayName: String,
    mailboxes: Seq[MailboxAddress]
  ) extends MailAddress {
    override def toString: String = MailAddress.safeString(displayName) + mailboxes.mkString(":", ",", ";")
  }
  
  sealed trait ListToken
  object ListToken {
    case class Str(str: String) extends ListToken
    case object Delimiter extends ListToken
    case object NameWildcard extends ListToken
    case object PathWildcard extends ListToken
  }
  sealed trait ListAttribute
  object ListAttribute {
    case object NoInferiors extends ListAttribute {
      override def toString = "\\Noinferiors"
    }
    case object NoSelect extends ListAttribute {
      override def toString = "\\Noselect"
    }
    case object Marked extends ListAttribute {
      override def toString = "\\Marked"
    }
    case object Unmarked extends ListAttribute {
      override def toString = "\\Unmarked"
    }
  }
  
  sealed trait SearchCriterion
  object SearchCriterion {
    case class SequenceSet(set: Imap.SequenceSet) extends SearchCriterion
    case object All extends SearchCriterion
    case object Answered extends SearchCriterion
    case class Bcc(string: String) extends SearchCriterion
    case class Before(date: LocalDate) extends SearchCriterion
    case class Body(string: String) extends SearchCriterion
    case class Cc(string: String) extends SearchCriterion
    case object Deleted extends SearchCriterion
    case object Draft extends SearchCriterion
    case object Flagged extends SearchCriterion
    case class From(string: String) extends SearchCriterion
    case class Header(fieldname: String, string: String) extends SearchCriterion
    case class Keyword(flag: Imap.Flag) extends SearchCriterion
    case class Larger(n: Long) extends SearchCriterion
    case object New extends SearchCriterion
    case class Not(searchKey: SearchCriterion) extends SearchCriterion
    case object Old extends SearchCriterion
    case class On(date: LocalDate) extends SearchCriterion
    case class Or(searchKey1: SearchCriterion, searchKey2: SearchCriterion) extends SearchCriterion
    case object Recent extends SearchCriterion
    case object Seen extends SearchCriterion
    case class SentBefore(date: LocalDate) extends SearchCriterion
    case class SentOn(date: LocalDate) extends SearchCriterion
    case class SentSince(date: LocalDate) extends SearchCriterion
    case class Since(date: LocalDate) extends SearchCriterion
    case class Smaller(n: Long) extends SearchCriterion
    case class Subject(string: String) extends SearchCriterion
    case class Text(string: String) extends SearchCriterion
    case class To(string: String) extends SearchCriterion
    case class Uid(set: Imap.SequenceSet) extends SearchCriterion
    case object Unanswered extends SearchCriterion
    case object Undeleted extends SearchCriterion
    case object Undraft extends SearchCriterion
    case object Unflagged extends SearchCriterion
    case class Unkeyword(flag: Imap.Flag) extends SearchCriterion
    case object Unseen extends SearchCriterion
  }
  
  sealed trait FlagOperation
  object FlagOperation {
    case object Replace extends FlagOperation
    case object Add extends FlagOperation
    case object Remove extends FlagOperation
  }
}