package scimap

object Imap {
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
  
  // TODO: Get more typesafe here (and too bad we can't be f-bounded here...have to have subtypes)
  sealed trait BodyStructureItem
  object BodyStructureItem {
    case object Nil extends BodyStructureItem
    case class Literal(value: String) extends BodyStructureItem
    case class List(values: Seq[BodyStructureItem]) extends BodyStructureItem
  }

  sealed trait BodyPart
  object BodyPart {
    case class Number(num: Int) extends BodyPart
    case object Header extends BodyPart
    case class HeaderFields(fields: Seq[String]) extends BodyPart
    case class HeaderFieldsNot(fields: Seq[String]) extends BodyPart
    case object Mime extends BodyPart
    case object Text extends BodyPart
  }
}