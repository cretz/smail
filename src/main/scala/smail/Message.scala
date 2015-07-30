package smail

import scala.util.Try
import java.time.format.DateTimeFormatter
import scala.util.Success
import java.time.Month
import java.time.DayOfWeek
import java.time.LocalDate
import java.time.ZoneOffset
import java.time.LocalTime
import java.time.OffsetTime
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatterBuilder

case class Message(fields: Seq[Message.Field], body: Option[String])
object Message {
  
  def fromString(string: String): Try[Message] = {
    import org.parboiled2._
    new Message.Parser(string).message.run()
  }
  
  //RFC5322 - 3.3
  lazy val DateFormat = new DateTimeFormatterBuilder().
    parseCaseInsensitive().
    appendPattern("dd-MMM-yyyy").
    toFormatter()
  lazy val DateTimeFormat = new DateTimeFormatterBuilder().
    parseCaseInsensitive().
    appendPattern("[EEE], dd MMM yyyy HH:mm:ss Z").
    toFormatter()
  
  sealed trait Address {
    override def toString: String = ???
  }
  object Address {
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
    
    def fromString(string: String): Try[Address] =
      new Parser(string).address.run()
    
    def multipleFromString(string: String): Try[Seq[Address]] =
      new Parser(string).addressList.run()    
    
    def multipleToString(addrs: Seq[Address], delimiter: String = ","): String =
      addrs.mkString(delimiter)

    case class Mailbox(
      mailbox: (String, String),
      displayName: Option[String] = None,
      atDomainList: Seq[String] = Seq.empty
    ) extends Address {
      override def toString: String = {
        val prefix =
          if (atDomainList.isEmpty) ""
          else atDomainList.map('@' + _).mkString(",") + ':'
        val addr = prefix + Address.safeString(mailbox._1) + '@' + Address.safeDomain(mailbox._2)
        displayName.map(Address.safeString).map(_ + " <" + addr + '>').getOrElse(addr)
      }
    }
    object Mailbox {
      def fromString(string: String): Try[Mailbox] =
        new Parser(string).mailbox.run()
      
      def multipleFromString(string: String): Try[Seq[Mailbox]] =
        new Parser(string).mailboxList.run()
    }
  
    case class Group(
      displayName: String,
      mailboxes: Seq[Mailbox]
    ) extends Address {
      override def toString: String = Address.safeString(displayName) + mailboxes.mkString(":", ",", ";")
    }
    object Group {
      def fromString(string: String): Try[Group] =
        new Parser(string).group.run()
    }
  }
  
  sealed trait Field {
    def prefix: String
    override def toString(): String = prefix + ": " + valueToString()
    def valueToString(): String
  }
  object Field {
    sealed abstract class SimpleField(val prefix: String) extends Field
    case class OrigDate(value: OffsetDateTime) extends SimpleField("Date") {
      override def valueToString(): String = DateTimeFormat.format(value)
    }
    case class From(value: Seq[Address.Mailbox]) extends SimpleField("From") {
      override def valueToString(): String = Address.multipleToString(value)
    }
    case class Sender(value: Address.Mailbox) extends SimpleField("Sender") {
      override def valueToString(): String = value.toString()
    }
    case class ReplyTo(value: Seq[Address]) extends SimpleField("Reply-To") {
      override def valueToString(): String = Address.multipleToString(value)
    }
    case class To(value: Seq[Address]) extends SimpleField("To") {
      override def valueToString(): String = Address.multipleToString(value)
    }
    case class Cc(value: Seq[Address]) extends SimpleField("Cc") {
      override def valueToString(): String = Address.multipleToString(value)
    }
    case class Bcc(value: Seq[Address]) extends SimpleField("Bcc") {
      override def valueToString(): String = Address.multipleToString(value)
    }
    case class MessageId(value: MsgId) extends SimpleField("Message-ID") {
      override def valueToString(): String = value.toString()
    }
    case class InReplyTo(value: Seq[MsgId]) extends SimpleField("In-Reply-To") {
      override def valueToString(): String = value.mkString(" ")
    }
    case class References(value: Seq[MsgId]) extends SimpleField("References") {
      override def valueToString(): String = value.mkString(" ")
    }
    case class Subject(value: String) extends SimpleField("Subject") {
      override def valueToString(): String = value
    }
    case class Comments(value: String) extends SimpleField("Comments") {
      override def valueToString(): String = value
    }
    case class Keywords(value: Seq[String]) extends SimpleField("Keywords") {
      override def valueToString(): String = value.mkString(",")
    }
    case class ResentDate(value: OffsetDateTime) extends SimpleField("Resent-Date") {
      override def valueToString(): String = DateTimeFormat.format(value)
    }
    case class ResentFrom(value: Seq[Address.Mailbox]) extends SimpleField("Resent-From") {
      override def valueToString(): String = Address.multipleToString(value)
    }
    case class ResentSender(value: Address.Mailbox) extends SimpleField("Resent-Sender") {
      override def valueToString(): String = value.toString()
    }
    case class ResentTo(value: Seq[Address]) extends SimpleField("Resent-To") {
      override def valueToString(): String = Address.multipleToString(value)
    }
    case class ResentCc(value: Seq[Address]) extends SimpleField("Resent-Cc") {
      override def valueToString(): String = Address.multipleToString(value)
    }
    case class ResentBcc(value: Seq[Address]) extends SimpleField("Resent-Bcc") {
      override def valueToString(): String = Address.multipleToString(value)
    }
    case class ResentMsgId(value: MsgId) extends SimpleField("Resent-Message-ID") {
      override def valueToString(): String = value.toString()
    }
    sealed trait Trace extends Field
    case class Return(value: Option[Address.Mailbox]) extends SimpleField("Return-Path") with Trace {
      override def valueToString(): String = value.map(_.toString).getOrElse("<>")
    }
    case class Received(
      value: Seq[Either[String, Address.Mailbox]],
      dateTime: OffsetDateTime
    ) extends SimpleField("Received") with Trace {
      override def valueToString(): String = {
        val ret = value.map({
          case Left(str) => str
          case Right(addr) => addr.toString
        }).mkString(" ")
        ret + ';' + DateTimeFormat.format(dateTime)
      }
    }
    case class Optional(name: String, value: String) extends SimpleField(name) {
      override def valueToString(): String = value
    }
  }
  
  case class DotAtomDetail(pre: Option[String], content: String, post: Option[String])
  case class MsgId(left: String, right: String) {
    override def toString(): String = '<' + left + '@' + right + '>'
  }
  
  private val shortMonthFormat = DateTimeFormatter.ofPattern("MMM")
  private val shortDayFormat = DateTimeFormatter.ofPattern("EEE")
  
  class Parser(val input: org.parboiled2.ParserInput) extends org.parboiled2.Parser {
    import org.parboiled2._
    
    // Helpers
    
    def charRange(start: Int, end: Int) = CharPredicate(start.toChar to end.toChar)
    
    def fieldHeader(name: String): Rule0 = rule { ignoreCase(name.toLowerCase) ~ ":" }
    
    // Keep in order of where they are in the RFC...add as needed
    
    // https://tools.ietf.org/html/rfc5234
    
    def alpha = rule { charRange(0x41, 0x5a) | charRange(0x61, 0x7a) }
    
    def bit = rule { ch('0') | '1' }
    
    def char = rule { charRange(0x01, 0x7f) }
    
    def cr = rule { ch(13) }
    
    def crlf = rule { cr ~ lf }
    
    def ctl = rule { charRange(0x00, 0x1f) | ch(0x7f) }
    
    def digit = rule { charRange(0x30, 0x39) }
    
    def dquote = rule { ch('"') }
    
    def hexDig = rule { digit | anyOf("ABCDEFabcdef") }
    
    def lf = rule { ch(10) }
    
    def vchar = rule { charRange(0x21, 0x7e) }
    
    def wsp = rule { anyOf(" \t") }
    
    // Ref: https://tools.ietf.org/html/rfc5322
    
    def quotedPair = rule { ch('\\') ~ (vchar | wsp) }
    
    def fws = rule { optional(zeroOrMore(wsp) ~ crlf) ~ oneOrMore(wsp) }
    
    def ctext = rule { charRange(33, 39) | charRange(42, 91) | charRange(93, 126) }
    
    def ccontent: Rule0 = rule { ctext | quotedPair | comment }
    
    def comment: Rule0 = rule { ch('(') ~ zeroOrMore(optional(fws) ~ ccontent) ~ optional(fws) ~ ')' }
    
    def cfws = rule { (oneOrMore(optional(fws) ~ comment) ~ optional(fws)) | fws }
    
    def atext = rule { alpha | digit | anyOf("!#$%&'*+-/=?^_`{|}~") }
    
    def atom = rule { optional(cfws) ~ oneOrMore(atext) ~ optional(cfws) }
    
    def dotAtomText = rule { oneOrMore(atext) ~ zeroOrMore("." ~ oneOrMore(atext)) }
    
    def dotAtom = rule { optional(cfws) ~ dotAtomText ~ optional(cfws) }
    
    def dotAtomWithDetail: Rule1[DotAtomDetail] =
      rule { optional(capture(cfws)) ~ capture(dotAtomText) ~ optional(capture(cfws)) ~> (
        (pre: Option[String], content: String, post: Option[String]) =>
          DotAtomDetail(pre, content, post)
      ) }
    
    def specials = rule { anyOf("()<>[]:;@\\,/") | dquote }
    
    def qtext = rule { ch(33) | charRange(35, 91) | charRange(93, 126) }
    
    def qcontent = rule { qtext | quotedPair }
    
    def quotedString = rule {
      optional(cfws) ~ dquote ~ zeroOrMore(optional(fws) ~ qcontent) ~ optional(fws) ~ dquote ~ optional(cfws)
    }
    
    def word = rule { atom | quotedString }
    
    def phrase = rule { oneOrMore(word) }
    
    def unstructured = rule { zeroOrMore(optional(fws) ~ vchar) ~ zeroOrMore(wsp) }
    
    // We could have used the DateTimeFormatter of MailDateFormat above, but this is more streaming
    
    def dateTime: Rule1[OffsetDateTime] = rule {
      optional(dayOfWeek ~ ',') ~ date ~ time ~ optional(cfws) ~> (
        (_: Option[DayOfWeek], d: LocalDate, t: OffsetTime) => t.atDate(d)
      )
    }
    
    def dayOfWeek = rule { optional(fws) ~ dayName }
    
    def dayName: Rule1[DayOfWeek] = rule {
      capture("Mon" | "Tue" | "Wed" | "Thu" | "Fri" | "Sat" | "Sun") ~> (
        (d: String) => DayOfWeek.from(shortDayFormat.parse(d))
      )
    }
    
    def date: Rule1[LocalDate] = rule { day ~ month ~ year ~> (
      (d: Int, m: Month, y: Int) => LocalDate.of(y, m, d)
    ) }
    
    def day: Rule1[Int] = rule { optional(fws) ~ capture(2.times(digit)) ~ fws ~> ((s: String) => s.toInt) }
    
    def month: Rule1[Month] = rule {
      capture("Jan" | "Feb" | "Mar" | "Apr" | "May" | "Jun" |
      "Jul" | "Aug" | "Sep" | "Oct" | "Nov" | "Dec") ~> (
        (m: String) => Month.from(shortMonthFormat.parse(m))
      )
    }
    
    def year: Rule1[Int] = rule { fws ~ capture(4.times(digit)) ~ fws ~> ((s: String) => s.toInt) }
    
    def time = rule { timeOfDay ~ zone ~> ((t, z) => OffsetTime.of(t, z)) }
    
    def timeOfDay = rule { hour ~ ':' ~ minute ~ optional(ch(':') ~ second) ~> (
      (h, m, s) => LocalTime.of(h, m, s.getOrElse(0))
    ) }
    
    def hour: Rule1[Int] = rule { capture(2.times(digit)) ~> ((s: String) => s.toInt) }
    
    def minute: Rule1[Int] = rule { capture(2.times(digit)) ~> ((s: String) => s.toInt) }
    
    def second: Rule1[Int] = rule { capture(2.times(digit)) ~> ((s: String) => s.toInt) }
    
    def zone: Rule1[ZoneOffset] = rule {
      fws ~ capture(ch('+') | '-') ~ capture(2.times(digit)) ~ capture(2.times(digit)) ~> (
        (sign: String, hour: String, minute: String) => {
          val hours = if (sign == "+") hour.toInt else -hour.toInt
          ZoneOffset.ofHoursMinutes(hours, minute.toInt)
        }
      )
    }
    
    def address: Rule1[Address] = rule { mailbox | group }
    
    def mailbox: Rule1[Address.Mailbox] = rule { nameAddr | addrSpec }
    
    def nameAddr = rule { optional(capture(displayName)) ~ angleAddr ~> (
      (displayName, address) => address.copy(displayName = displayName.map(_.trim))
    ) }
    
    def angleAddr = rule {
      (optional(cfws) ~ '<' ~ addrSpec ~ '>' ~ optional(cfws)) | obsAngleAddr
    }
    
    def group: Rule1[Address.Group] = rule {
      capture(displayName) ~ ':' ~ optional(groupList) ~ ';' ~ optional(cfws) ~> (
        (displayName: String, mailboxes: Option[Seq[Address.Mailbox]]) =>
          Address.Group(displayName.trim, mailboxes.getOrElse(Seq.empty))
      )
    }
    
    def displayName = rule { phrase }
    
    def mailboxList: Rule1[Seq[Address.Mailbox]] = rule { oneOrMore(mailbox).separatedBy(",") }
    
    def addressList = rule { oneOrMore(address).separatedBy(",") }
    
    def groupList: Rule1[Seq[Address.Mailbox]] = rule { mailboxList | (cfws ~> (() => Seq.empty)) }
    
    def addrSpec: Rule1[Address.Mailbox] = rule { localPart ~ '@' ~ domain ~> (
      (local: String, domain: DotAtomDetail) =>
        // Legacy implementations allow for trailing comment to be used as display name
        Address.Mailbox(
          local.trim -> domain.content.trim,
          domain.post.map(_.trim.stripPrefix("(").stripSuffix(")"))
        )
    ) }
    
    def localPart: Rule1[String] = rule { (dotAtomWithDetail ~> (_.content)) | capture(quotedString) }
    
    def domain: Rule1[DotAtomDetail] = rule {
      dotAtomWithDetail |
      (capture(domainLiteral) ~> (DotAtomDetail(None, _: String, None)))
    }
    
    def domainLiteral = rule {
      optional(cfws) ~ '[' ~ zeroOrMore(optional(fws) ~ dtext) ~ optional(fws) ~ ']' ~ optional(cfws)
    }
    
    def dtext = rule { charRange(33, 90) | charRange(94, 126) }
    
    def message: Rule1[Message] = rule { fields ~ optional(crlf ~ capture(body)) ~> ((f, b) => Message(f, b)) }
    
    def body = rule {
      // TODO: limit to 998 chars
      zeroOrMore(zeroOrMore(text) ~ crlf) ~ zeroOrMore(text)
    }
    
    def text = rule { charRange(1, 9) | ch(11) | ch(12) | charRange(14, 127) }
    
    def resentField: Rule1[Field] = rule {
      resentDate | resentFrom | resentSender | resentTo |
      resentCc | resentBcc | resentMsgId
    }
    
    def traceFields = rule {
      trace ~ zeroOrMore(resentField | optionalField) ~> ((t, o) => t ++ o)
    }
    
    def otherFields = rule {
      origDate | from | sender | replyTo | to | cc | bcc | messageId |
      inReplyTo | references | subject | comments | keywords | optionalField
    }
    
    def fields = rule { zeroOrMore(traceFields) ~ zeroOrMore(otherFields) ~> (
      (t, f) => t.flatten ++ f
    ) }
    
    def origDate = rule { fieldHeader("Date") ~ dateTime ~ crlf ~> (Field.OrigDate(_)) }
    
    def from = rule { fieldHeader("From") ~ mailboxList ~ crlf ~> (Field.From(_)) }
    
    def sender = rule { fieldHeader("Sender") ~ mailbox ~ crlf ~> (Field.Sender(_)) }
    
    def replyTo = rule { fieldHeader("Reply-To") ~ addressList ~ crlf ~> (Field.ReplyTo(_)) }
    
    def to = rule { fieldHeader("To") ~ addressList ~ crlf ~> (Field.To(_)) }
    
    def cc = rule { fieldHeader("Cc") ~ addressList ~ crlf ~> (Field.Cc(_)) }
    
    def bcc = rule {
      fieldHeader("Bcc") ~ (addressList | (cfws ~> (() => Seq.empty))) ~ crlf ~> (Field.Bcc(_))
    }
    
    def messageId = rule { fieldHeader("Message-ID") ~ msgId ~ crlf ~> (Field.MessageId(_)) }
    
    def inReplyTo = rule { fieldHeader("In-Reply-To") ~ oneOrMore(msgId) ~ crlf ~> (Field.InReplyTo(_)) }
    
    def references = rule { fieldHeader("References") ~ oneOrMore(msgId) ~ crlf ~> (Field.References(_)) }
    
    def msgId = rule { optional(cfws) ~ '<' ~ capture(idLeft) ~ '@' ~ capture(idRight) ~ '>' ~ optional(cfws) ~> (
      (left, right) => MsgId(left, right)
    ) }
    
    def idLeft = rule { dotAtomText }
    
    def idRight = rule { dotAtomText | noFoldLiteral }
    
    def noFoldLiteral = rule { ch('[') ~ zeroOrMore(dtext) ~ ']' }
    
    def subject = rule { fieldHeader("Subject") ~ capture(unstructured) ~ crlf ~> (
      (s: String) => Field.Subject(s.trim)
    ) }
    
    def comments = rule { fieldHeader("Comments") ~ capture(unstructured) ~ crlf ~> (
      (s: String) => Field.Comments(s.trim)
    ) }
    
    def keywords = rule {
      fieldHeader("Keywords") ~ oneOrMore(capture(phrase)).separatedBy(",") ~ crlf ~> (Field.Keywords(_))
    }
    
    def resentDate = rule { fieldHeader("Resent-Date") ~ dateTime ~ crlf ~> (Field.ResentDate(_)) }
    
    def resentFrom = rule { fieldHeader("Resent-From") ~ mailboxList ~ crlf ~> (Field.ResentFrom(_)) }
    
    def resentSender = rule { fieldHeader("Resent-Sender") ~ mailbox ~ crlf ~> (Field.ResentSender(_)) }
    
    def resentTo = rule { fieldHeader("Resent-To") ~ addressList ~ crlf ~> (Field.ResentTo(_)) }
    
    def resentCc = rule { fieldHeader("Resent-Cc") ~ addressList ~ crlf ~> (Field.ResentCc(_)) }
    
    def resentBcc = rule {
      fieldHeader("Resent-Bcc") ~ ((addressList ~> (Some(_))) | (cfws ~> (() => None))) ~ crlf ~> (
        (addrs: Option[Seq[Address]]) => Field.ResentBcc(addrs.getOrElse(Seq.empty))
      )
    }
    
    def resentMsgId = rule { fieldHeader("Resent-Message-ID") ~ msgId ~ crlf ~> (Field.ResentMsgId(_)) }
    
    def trace: Rule1[Seq[Field.Trace]] = rule { optional(`return`) ~ oneOrMore(received) ~> (
      (ret: Option[Field.Return], rec: Seq[Field.Received]) => ret.toSeq ++ rec
    ) }
    
    def `return` = rule { fieldHeader("Return-Path") ~ path ~ crlf ~> (Field.Return(_)) }
    
    def path: Rule1[Option[Address.Mailbox]] = rule {
      (angleAddr ~> (Some(_))) |
      (optional(cfws) ~ '<' ~ optional(cfws) ~ '>' ~ optional(cfws) ~> (() => None))
    }
    
    def received = rule { fieldHeader("Received") ~ zeroOrMore(receivedToken) ~ ';' ~ dateTime ~ crlf ~> (
      (tokens, dateTime) => Field.Received(tokens, dateTime)
    ) }
    
    def receivedToken: Rule1[Either[String, Address.Mailbox]] = rule {
      (capture(word) ~> (Left(_))) |
      (angleAddr ~> (Right(_))) |
      (addrSpec ~> (Right(_))) |
      (domain ~> ((d: DotAtomDetail) => Left(d.content)))
    }
    
    def optionalField = rule { capture(fieldName) ~ ':' ~ capture(unstructured) ~ crlf ~> (
      (name, value) => Field.Optional(name, value)
    ) }
    
    def fieldName = rule { oneOrMore(ftext) }
    
    def ftext = rule { charRange(33, 57) | charRange(59, 126) }
    
    def obsAngleAddr = rule { optional(cfws) ~ '<' ~ obsRoute ~ addrSpec ~ '>' ~ optional(cfws) ~>
      ((domains, addr) => addr.copy(atDomainList = domains))
    }
    
    def obsRoute = rule { obsDomainList ~ ':' }
    
    def obsDomainList: Rule1[Seq[String]] = rule {
      zeroOrMore(optional(cfws) ~ '@' ~ (domain ~> (_.content))).separatedBy(",")
    }
  }
  
}