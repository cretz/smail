package smail.imap

import java.time.ZonedDateTime
import scala.reflect.runtime.universe._

trait MailHeaders {
  import MailHeaders._
  
  def headers(): Map[Type[_], Any]
  def apply[T](t: Type[T]): Option[T]
  def +[T](v: (Type[T], T)): MailHeaders
  def ++[T](v: (SeqType[T], T)): MailHeaders
  def lines[T](t: Type[T]): Seq[String] = apply(t).toSeq.flatMap(t.toLines)
  override def toString(): String =
    headers().flatMap({ case (typ, v) => lines(typ) }).mkString("\r\n")
}
object MailHeaders {
  
  private class InMemory(internalHeaders: Map[Type[_], Any] = Map.empty) extends MailHeaders {
    override def headers(): Map[Type[_], Any] = internalHeaders
    override def apply[T](t: Type[T]): Option[T] = headers.get(t).map(_.asInstanceOf[T])
    override def +[T](v: (Type[T], T)) = new InMemory(internalHeaders + v)
    override def ++[T](v: (SeqType[T], T)) = internalHeaders.get(v._1) match {
      case Some(s: Seq[Any]) => new InMemory(internalHeaders + (v._1 -> (s :+ v._2)))
      case None => new InMemory(internalHeaders + (v._1 -> Seq.empty))
      case _ => sys.error("Strange type")
    }
  }
  object InMemory {
    lazy val empty: MailHeaders = new InMemory()
    def apply(): MailHeaders = empty
  }
  
  private var types = Map.empty[String, Type[_]]
  private def registerType[U <: Type[_]](typ: U): U = {
    val cased = typ.name.toUpperCase
    require(!types.contains(cased), "Type already exists")
    types += cased -> typ
    typ
  }
  def typeFromString[_](str: String): Option[Type[_]] = types.get(str.toUpperCase)

  class Type[T : TypeTag] private[imap] (val name: String) {
    val delimiter = ","

    protected def anyValueToString(v: Any): Option[String] = v match {
      case d: ZonedDateTime => Some(Imap.mailDateTimeFormat.format(d))
      case (lhs, rhs) => anyValueToString(lhs).flatMap(l => anyValueToString(rhs).map(l + delimiter + _))
      case Seq() => None
      case s: Seq[_] =>
        val seq = s.flatMap(anyValueToString(_).toSeq)
        if (seq.isEmpty) None else Some(seq.mkString(delimiter))
      case s => Some(s.toString)
    }

    def valueToString(v: T): Seq[String] = anyValueToString(v).toSeq

    def toLines(v: T): Seq[String] = valueToString(v).map(name + ": " + _)
    
    def valueFromString(v: String): T = {
      val ret = typeOf[T] match {
        case t if t =:= typeOf[ZonedDateTime] => ZonedDateTime.parse(v, Imap.mailDateTimeFormat)
        case t if t =:= typeOf[(String, String)] =>
          val index = v.indexOf(delimiter)
          require(index != -1)
          v.take(index) -> v.drop(index + 1)
        case t if t =:= typeOf[Imap.MailboxAddress] =>
          Imap.MailboxAddress.fromString(v)
        case t if t =:= typeOf[Imap.MailAddress] =>
          Imap.MailAddress.fromString(v)
        case t if t =:= typeOf[Seq[_]] =>
          if (v.isEmpty) Seq()
          else v.split(delimiter).map(valueFromString)
        case t if t =:= typeOf[String] =>
          v
        case t => sys.error("Unrecognized type: " + t)
      }
      ret.asInstanceOf[T]
    }
  }
  object Type {
    def apply[T : TypeTag](name: String): Type[T] = registerType { new Type[T](name) }
    def apply[T : TypeTag](name: String, valToStr: T => Seq[String]): Type[T] = registerType {
      new Type[T](name) {
        override def valueToString(v: T) = valToStr(v)
      }
    }
  }

  class SeqType[T : TypeTag] private[imap] (private val nme: String) extends Type[Seq[T]](nme) {
    override def valueToString(v: Seq[T]): Seq[String] = v.flatMap(anyValueToString(_).toSeq)
  }
  object SeqType {
    def apply[T : TypeTag](name: String): SeqType[T] = registerType { new SeqType[T](name) }
  }
  
  val ReturnPath = Type[String]("Return-Path")
  val Received = SeqType[String]("Received")
  val ResentDate = SeqType[ZonedDateTime]("Resent-Date")
  val ResentFrom = SeqType[Seq[Imap.MailAddress]]("Resent-From")
  val ResentSender = SeqType[Imap.MailboxAddress]("Resent-Sender")
  val ResentTo = SeqType[Seq[Imap.MailboxAddress]]("Resent-To")
  val ResentCc = SeqType[Seq[Imap.MailboxAddress]]("Resent-Cc")
  val ResentBcc = SeqType[Seq[Imap.MailboxAddress]]("Resent-Bcc")
  val ResentMessageId = SeqType[(String, String)]("Resent-Message-ID")
  val Date = Type[ZonedDateTime]("Date")
  val From = Type[Seq[Imap.MailAddress]]("From")
  val Sender = Type[Imap.MailAddress]("Sender")
  val ReplyTo = Type[Seq[Imap.MailAddress]]("Reply-To")
  val To = Type[Seq[Imap.MailAddress]]("To")
  val Cc = Type[Seq[Imap.MailAddress]]("Cc")
  val Bcc = Type[Seq[Imap.MailAddress]]("Bcc")
  val MessageId = Type[(String, String)](
    "Message-ID",
    (id: (String, String)) => Seq('<' + id._1 + '@' + id._2 + '>')
  )
  val InReplyTo = Type[Seq[(String, String)]](
    "In-Reply-To",
    (ids: Seq[(String, String)]) => Seq(ids.flatMap(MessageId.valueToString).mkString(" "))
  )
  val References = Type[Seq[(String, String)]]("References", InReplyTo.valueToString(_))
  val Subject = Type[String]("Subject")
  val Comments = SeqType[String]("Comments")
  val Keywords = SeqType[String]("Keywords")
  val Newsgroups = Type[String]("Newsgroups")
}