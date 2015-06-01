package scimap

import akka.stream.stage.StatefulStage
import akka.stream.stage.Context
import akka.stream.stage.SyncDirective

trait ServerResponseToString {
  import ServerResponse._
  
  def serverResponse(response: ServerResponse): String = response match {
    case r: StatusResponse => statusResponse(r)
    case r: MailboxStatusResponse => mailboxStatusResponse(r)
    case r: MailboxSizeResponse => mailboxSizeResponse(r)
    case r: MessageStatusResponse => messageStatusResponse(r)
    case Continuation(text) => "+ " + text.getOrElse("")
  }
  
  def messageStatusResponse(resp: MessageStatusResponse): String = resp match {
    case Expunge(seq) => s"* $seq EXPUNGE"
    case Fetch(seq, data) => s"* $seq FETCH (" + data.map(fetchDataItem).mkString(" ") + ")"
  }
  
  def fetchDataItem(item: FetchDataItem): String = {
    import FetchDataItem._
    item match {
      case Body(None, contents, _) => "BODY " + contents
      case Body(Some(section), contents, origin) =>
        "BODY[" + section.map(bodyPart).mkString(".") + "]" +
          origin.map("<" + _ + ">").getOrElse("") + " " + contents
      case BodyStructure(list) => "BODYSTRUCTURE " + bodyStructureItem(list)
      case Envelope(list) => "ENVELOPE " + bodyStructureItem(list)
      case Flags(flags) => "FLAGS (" + flags.mkString(" ") + ")"
      case InternalDate(date) => "INTERNALDATE " + TokenSetToClientCommand.dateTimeFormat.print(date)
      case Rfc822(contents) => "RFC822 " + contents
      case Rfc822Header(contents) => "RFC822.HEADER " + contents
      case Rfc822Size(size) => "RFC822.SIZE " + size
      case Rfc822Text(contents) => "RFC822.TEXT " + contents
      case Uid(uid) => "UID " + uid
    }
  }
  
  def bodyStructureItem(item: FetchDataItem.BodyStructureItem): String = {
    import FetchDataItem.BodyStructureItem._
    item match {
      case Literal(value) => safeString(value)
      case List(values) => "(" + values.map(bodyStructureItem) + ")"
    }
  }
  
  def bodyPart(part: FetchDataItem.BodyPart): String = {
    import FetchDataItem.BodyPartSpecifier._
    part match {
      case Left(int) => int.toString
      case Right(Header) => "HEADER"
      case Right(HeaderFields(fields)) => "HEADER.FIELDS (" + fields.mkString(" ") + ')'
      case Right(HeaderFieldsNot(fields)) => "HEADER.FIELDS.NOT (" + fields.mkString(" ") + ')'
      case Right(Mime) => "MIME"
      case Right(Text) => "TEXT"
    }
  }
  
  def mailboxSizeResponse(resp: MailboxSizeResponse): String = resp match {
    case Exists(count) => s"* $count EXISTS"
    case Recent(count) => s"* $count RECENT"
  }
  
  def mailboxStatusResponse(resp: MailboxStatusResponse): String = resp match {
    case Capability(names) =>
      "* CAPABILITY " + names.map(capability).mkString(" ")
    case List(name, delimiter, nameAttributes) =>
      "* LIST (" + nameAttributes.mkString(" ") + ") " +
        delimiter.map(d => safeString(d.toString)).getOrElse("NIL") + safeString(name)
    case LSub(name, delimiter, nameAttributes) =>
      "* LSUB (" + nameAttributes.mkString(" ") + ") " +
        delimiter.map(d => safeString(d.toString)).getOrElse("NIL") + safeString(name)
    case Status(name, info) =>
      s"* STATUS $name (" + info.map(i => i._1 + " " + i._2).mkString(" ") + ')'
    case Search(numbers) =>
      "* SEARCH " + numbers.mkString(" ")
    case Flags(flags) =>
      "* FLAGS (" + flags.mkString(" ") + ')'
  }
  
  def statusResponse(resp: StatusResponse): String = {
    val tag = resp.tag.getOrElse("*")
    val responseCode = resp.responseCode.map(" [" + statusResponseCode(_) + ']').getOrElse("")
    tag + ' ' + resp.name + responseCode + ' ' + safeString(resp.text)
  }
  
  def statusResponseCode(code: StatusResponseCode): String = {
    import StatusResponseCode._
    code match {
      case Alert(msg) => "ALERT " + safeString(msg)
      case BadCharset(Seq()) => "BADCHARSET"
      case BadCharset(charsets) => "BADCHARSET " + charsets.map(safeString).mkString(",")
      case Capability(names) => "CAPABILITY " + names.map(capability).mkString(" ")
      case Parse(msg) => "PARSE " + safeString(msg)
      case PermanentFlags(flags) => "PERMANENTFLAGS " + flags.mkString(" ")
      case ReadOnly => "READ-ONLY"
      case ReadWrite => "READ-WRITE"
      case TryCreate => "TRYCREATE"
      case UidNext(value) => s"UIDNEXT $value"
      case UidValidity(value) => s"UIDVALIDITY $value"
      case Unseen(value) => s"UNSEEN $value"
    }
  }
  
  def capability(name: CapabilityName): String = {
    import CapabilityName._
    name match {
      case Imap4Rev1 => "IMAP4rev1"
      case StartTls => "STARTTLS"
      case LoginDisabled => "LOGINDISABLED"
      case Auth(mechanism) => s"AUTH=$mechanism"
      case Custom(contents, true) => s"X$contents"
      case Custom(contents, false) => contents
    }
  }
  
  def safeString(string: String): String = {
    // Only if the string contains a slash or a double quote do we want to double quote it
    val result = string.replace("\\", "\\\\").replace("\"","\\\"")
    if (result.length > 0 && result.length == string.length) result
    else '"' + result + '"'
  }
}

object ServerResponseToString {
  class Stage extends StatefulStage[ServerResponse, String] with ServerResponseToString {
    override def initial = new State {
      override def onPush(chunk: ServerResponse, ctx: Context[String]): SyncDirective = {
        emit(Iterator.single(serverResponse(chunk)), ctx)
      }
    }
  }
}