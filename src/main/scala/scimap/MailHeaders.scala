package scimap

import java.time.ZonedDateTime

trait MailHeaders {
  import MailHeaders._
  
  def headers(): Map[Type[_], Any]
  def apply[T](t: Type[T]): Option[T]
  def +[T](v: (Type[T], T)): MailHeaders
}
object MailHeaders {
  
  private class InMemory(internalHeaders: Map[Type[_], Any] = Map.empty) extends MailHeaders {
    override def headers(): Map[Type[_], Any] = internalHeaders
    override def apply[T](t: Type[T]): Option[T] = headers.get(t).map(_.asInstanceOf[T])
    override def +[T](v: (Type[T], T)) = new InMemory(internalHeaders + v)
  }
  object InMemory {
    lazy val empty: MailHeaders = new InMemory()
    def apply(): MailHeaders = empty
  }
  
  sealed trait Type[T] {
    def toString(v: T): String = ???
  }
  case object Trace extends Type[NotYetImplemented]
  case object ResentDate extends Type[NotYetImplemented]
  case object ResentFrom extends Type[NotYetImplemented]
  case object ResentSender extends Type[NotYetImplemented]
  case object ResentTo extends Type[NotYetImplemented]
  case object ResentCc extends Type[NotYetImplemented]
  case object ResentBcc extends Type[NotYetImplemented]
  case object ResentMsgId extends Type[NotYetImplemented]
  case object OrigDate extends Type[ZonedDateTime] {
    override def toString(v: ZonedDateTime) = "Date: " + Imap.mailDateTimeFormat.format(v)
  }
  case object From extends Type[Seq[Imap.MailAddress]]
  case object Sender extends Type[Imap.MailAddress]
  case object ReplyTo extends Type[Seq[Imap.MailAddress]]
  case object To extends Type[Seq[Imap.MailAddress]]
  case object Cc extends Type[Seq[Imap.MailAddress]]
  case object Bcc extends Type[Seq[Imap.MailAddress]]
  case object MessageId extends Type[(String, String)]
  case object InReplyTo extends Type[(String, String)]
  case object References extends Type[NotYetImplemented]
  case object Subject extends Type[String] {
    override def toString(v: String) = "Subject: " + v
  }
  case object Comments extends Type[NotYetImplemented]
  case object Keywords extends Type[NotYetImplemented]
  case object OptionalField extends Type[NotYetImplemented]
  
  case class NotYetImplemented() { ??? }
}