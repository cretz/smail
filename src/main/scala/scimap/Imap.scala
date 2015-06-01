package scimap

object Imap {
  case class SequenceSet(items: Seq[SequenceSetItem])
  sealed trait SequenceSetItem
  case class SequenceRange(low: SequenceNumber, high: SequenceNumber) extends SequenceSetItem
  sealed trait SequenceNumber extends SequenceSetItem
  case class SequenceNumberLiteral(value: Long) extends SequenceNumber
  case object SequenceNumberAll extends SequenceNumber
  
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
}