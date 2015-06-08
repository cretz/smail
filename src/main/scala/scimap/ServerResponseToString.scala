package scimap

trait ServerResponseToString extends (ServerResponse => String) {
  import ServerResponse._
  
  def apply(response: ServerResponse): String = serverResponse(response)
  
  def serverResponse(response: ServerResponse): String = response match {
    case r: StatusResponse => statusResponse(r)
    case r: MailboxStatusResponse => mailboxStatusResponse(r)
    case r: MailboxSizeResponse => mailboxSizeResponse(r)
    case r: MessageStatusResponse => messageStatusResponse(r)
    case Continuation(text) => "+ " + text.getOrElse("")
    case CloseConnection | StartTls => sys.error("This should be intercepted by the stage")
  }
  
  def messageStatusResponse(resp: MessageStatusResponse): String = resp match {
    case Expunge(seq) => s"* $seq EXPUNGE"
    case Fetch(seq, data) => s"* $seq FETCH (" + data.map(fetchDataItem).mkString(" ") + ")"
  }
  
  def fetchDataItem(item: FetchDataItem): String = {
    import FetchDataItem._
    item match {
      case NonExtensibleBodyStructure(list) => "BODY " + imapToken(list)
      case Body(part, contents, origin) =>
        "BODY[" + bodyPart(part) + "]" + origin.map("<" + _ + ">").getOrElse("") + " " + literalString(contents)
      case BodyStructure(list) => "BODYSTRUCTURE " + imapToken(list)
      case Envelope(list) => "ENVELOPE " + imapToken(list)
      case Flags(flags) => "FLAGS (" + flags.mkString(" ") + ")"
      case InternalDate(date) => "INTERNALDATE " + safeString(Imap.dateTimeFormat.format(date))
      case Rfc822(contents) => "RFC822 " + literalString(contents)
      case Rfc822Header(contents) => "RFC822.HEADER " + literalString(contents)
      case Rfc822Size(size) => "RFC822.SIZE " + size
      case Rfc822Text(contents) => "RFC822.TEXT " + literalString(contents)
      case Uid(uid) => "UID " + uid
    }
  }
  
  def imapToken(token: ImapToken): String = {
    import ImapToken._
    token match {
      case Nil => "NIL"
      case Str(value, true) =>
        val str = safeString(value)
        if (str.charAt(0) == '"') str else '"' + str + '"'
      case Str(value, _) => safeString(value)
      case List('(', values) => '(' + values.map(imapToken).mkString(" ") + ')'
      case tok => sys.error("Unsupported token: " + tok)
    }
  }
  
  def bodyPart(part: Imap.BodyPart): String = {
    import Imap.BodyPart._
    part match {
      case Part(nums) => nums.mkString(".")
      case Header(nums) => nums.mkString("", ".", ".HEADER")
      case HeaderFields(nums, fields) => nums.mkString("", ".", ".HEADER.FIELDS (" + fields.mkString(" ") + ')')
      case HeaderFieldsNot(nums, fields) => nums.mkString("", ".", ".HEADER.FIELDS.NOT (" + fields.mkString(" ") + ')')
      case Mime(nums) => nums.mkString("", ".", ".MIME")
      case Text(nums) => nums.mkString("", ".", ".TEXT")
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
      case PermanentFlags(flags) => "PERMANENTFLAGS (" + flags.mkString(" ") + ")"
      case ReadOnly => "READ-ONLY"
      case ReadWrite => "READ-WRITE"
      case TryCreate => "TRYCREATE"
      case UidNext(value) => s"UIDNEXT $value"
      case UidValidity(value) => s"UIDVALIDITY $value"
      case Unseen(value) => s"UNSEEN $value"
    }
  }
  
  def capability(name: Imap.Capability): String = {
    import Imap.Capability._
    name match {
      case Imap4Rev1 => "IMAP4rev1"
      case StartTls => "STARTTLS"
      case LoginDisabled => "LOGINDISABLED"
      case Auth(mechanism) => s"AUTH=$mechanism"
      case Custom(contents, true) => s"X$contents"
      case Custom(contents, false) => contents
    }
  }
  
  def literalString(string: String): String = {
    return "{" + string.length + s"}\r\n$string"
  }
  
  def safeString(string: String): String = {
    // Only if the string contains a slash, double quote, or a space do we want to double quote it
    val result = string.replace("\\", "\\\\").replace("\"","\\\"")
    if (result.length > 0 && result.length == string.length && result.indexOf(' ') == -1) result
    else '"' + result + '"'
  }
}

object ServerResponseToString {
  def apply(): ServerResponseToString = new ServerResponseToString() { }
}