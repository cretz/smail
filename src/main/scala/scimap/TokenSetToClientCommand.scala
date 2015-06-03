package scimap

import akka.stream.stage.StatefulStage
import akka.util.ByteString
import akka.stream.stage.SyncDirective
import akka.stream.stage.Context
import org.joda.time.format.DateTimeFormat
import scala.util.Try

trait TokenSetToClientCommand {
  import ClientCommand._
  
  var waitingForText = Seq.empty[ImapToken]
  
  def getClientCommand(chunk: Seq[ImapToken]): ParseResult = {
    var properChunk = waitingForText ++ chunk
    waitingForText = Seq.empty
    properChunk.headOption match {
      case Some(ImapToken.Str(tag)) => properChunk.last match {
        case _: ImapToken.StrCountPrefix =>
          waitingForText = properChunk.dropRight(1)
          WaitingForMoreText(properChunk)
        case _ => properChunk.lift(1) match {
          // The next one has to be a string too
          case Some(ImapToken.Str(cmdName)) =>
            parseCommand(tag, cmdName, properChunk.drop(2), properChunk)
          case _ => UnrecognizedCommand(properChunk)
        }
      }
      case _ => UnknownFailure("Unrecognized tag", properChunk)
    }
  }

  def parseCommand(
    tag: String,
    cmdName: String,
    parameters: Seq[ImapToken],
    allTokens: Seq[ImapToken]
  ): ParseResult = cmdName match {
    case "CAPABILITY" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(Capability(tag))
    case "NOOP" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(Noop(tag))
    case "LOGOUT" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(Logout(tag))
    case "STARTTLS" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(StartTls(tag))
    case "AUTHENTICATE" => parameters match {
      case Seq(ImapToken.Str(mechanism)) => CommandSuccess(Authenticate(tag, mechanism))
      case _ => UnexpectedArguments(allTokens)
    }
    case "LOGIN" => parameters match {
      case Seq(ImapToken.Str(username), ImapToken.Str(password)) => CommandSuccess(Login(tag, username, password))
      case _ => UnexpectedArguments(allTokens)
    }
    case "SELECT" => parameters match {
      case Seq(ImapToken.Str(mailbox)) => CommandSuccess(Select(tag, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case "EXAMINE" => parameters match {
      case Seq(ImapToken.Str(mailbox)) => CommandSuccess(Examine(tag, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case "CREATE" => parameters match {
      case Seq(ImapToken.Str(mailbox)) => CommandSuccess(Create(tag, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case "DELETE" => parameters match {
      case Seq(ImapToken.Str(mailbox)) => CommandSuccess(Delete(tag, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case "RENAME" => parameters match {
      case Seq(ImapToken.Str(existingMailbox), ImapToken.Str(newMailbox)) =>
        CommandSuccess(Rename(tag, existingMailbox, newMailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case "SUBSCRIBE" => parameters match {
      case Seq(ImapToken.Str(mailbox)) => CommandSuccess(Subscribe(tag, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case "UNSUBSCRIBE" => parameters match {
      case Seq(ImapToken.Str(mailbox)) => CommandSuccess(Unsubscribe(tag, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case "LIST" => parameters match {
      case Seq(ImapToken.Str(reference), ImapToken.Str(mailbox)) => CommandSuccess(List(tag, reference, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case "LSUB" => parameters match {
      case Seq(ImapToken.Str(reference), ImapToken.Str(mailbox)) => CommandSuccess(LSub(tag, reference, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case "STATUS" => parameters match {
      case Seq(ImapToken.Str(mailbox), ImapToken.List('(', dataItems)) =>
        dataItems.foldLeft(CommandSuccess(Status(tag, mailbox, Seq.empty)): ParseResult) {
          case (CommandSuccess(s: Status), ImapToken.Str(dataItem)) => dataItem match {
            case "MESSAGES" => CommandSuccess(s.copy(dataItems = s.dataItems :+ Imap.StatusDataItem.Messages))
            case "RECENT" => CommandSuccess(s.copy(dataItems = s.dataItems :+ Imap.StatusDataItem.Recent))
            case "UIDNEXT" => CommandSuccess(s.copy(dataItems = s.dataItems :+ Imap.StatusDataItem.UidNext))
            case "UIDVALIDITY" => CommandSuccess(s.copy(dataItems = s.dataItems :+ Imap.StatusDataItem.UidValidity))
            case "UNSEEN" => CommandSuccess(s.copy(dataItems = s.dataItems :+ Imap.StatusDataItem.Unseen))
            case _ => UnexpectedArguments(allTokens)
          }
          case (res, _) => res
        }
      case _ => UnexpectedArguments(allTokens)
    }
    case "APPEND" =>
      val pieces = parameters match {
        case Seq(ImapToken.Str(mailbox), ImapToken.List('(', flags),
          ImapToken.Str(dateTime), ImapToken.Str(message)) => Some((mailbox, flags, Some(dateTime), message))
        case Seq(ImapToken.Str(mailbox), ImapToken.List('(', flags), ImapToken.Str(message)) =>
          Some((mailbox, flags, None, message))
        case Seq(ImapToken.Str(mailbox), ImapToken.Str(dateTime), ImapToken.Str(message)) =>
          Some((mailbox, Seq.empty, Some(dateTime), message))
        case Seq(ImapToken.Str(mailbox), ImapToken.Str(message)) => Some((mailbox, Seq.empty, None, message))
        case _ => None
      }
      pieces match {
        case None => UnexpectedArguments(allTokens)
        case Some((mailbox, flags, dateTime, message)) =>
          flags.foldLeft(Option(Seq.empty[Imap.Flag])) {
            case (None, _) => None
            case (Some(seq), flag) => flagFromToken(flag).map(seq :+ _)
          } match {
            case Some(flags) => Try(dateTime.map(TokenSetToClientCommand.dateTimeFormat.parseDateTime)) match {
              case scala.util.Success(dateTime) => CommandSuccess(Append(tag, mailbox, message, flags, dateTime))
              case _ => UnexpectedArguments(allTokens)
            }
            case None => UnexpectedArguments(allTokens)
          }
      }
    case "CHECK" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(Check(tag))
    case "CLOSE" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(Close(tag))
    case "EXPUNGE" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(Expunge(tag))
    case "SEARCH" =>
      val (charset, searchParameters) = parameters.take(2) match {
        case Seq(ImapToken.Str("CHARSET"), ImapToken.Str(charset)) => Some(charset) -> parameters.drop(2)
        case _ => None -> parameters
      }
      searchCriterias(searchParameters) match {
        case Some(crits) => CommandSuccess(Search(tag, crits, charset))
        case None => UnexpectedArguments(allTokens)
      }
    case "FETCH" =>
      parameters.headOption.collect({
        case ImapToken.Str(string) => sequenceSetFromString(string) 
      }).flatten match {
        case None => UnexpectedArguments(allTokens)
        case Some(set) => fetchItemFromTokens(parameters.drop(1)) match {
          case None => UnexpectedArguments(allTokens)
          case Some(item) => CommandSuccess(Fetch(tag, set, item))
        }
      }
    // TODO: here and elsewhere, let's check that there aren't extra params
    case "STORE" =>
      parameters.headOption.collect({
        case ImapToken.Str(string) => sequenceSetFromString(string) 
      }).flatten match {
        case None => UnexpectedArguments(allTokens)
        case Some(set) => parameters.lift(2) match {
          case Some(ImapToken.List('(', flagList)) =>
            flagList.foldLeft(Option(Seq.empty[Imap.Flag])) {
              case (None, _) => None
              case (Some(seq), flag) => flagFromToken(flag).map(seq :+ _)
            } match {
              case None => UnexpectedArguments(allTokens)
              case Some(flags) => parameters(1) match {
                case ImapToken.Str("FLAGS") =>
                  CommandSuccess(Store(tag, set, StoreDataItem(flags, StoreDataItem.FlagOperation.Replace, false)))
                case ImapToken.Str("FLAGS.SILENT") =>
                  CommandSuccess(Store(tag, set, StoreDataItem(flags, StoreDataItem.FlagOperation.Replace, true)))
                case ImapToken.Str("+FLAGS") =>
                  CommandSuccess(Store(tag, set, StoreDataItem(flags, StoreDataItem.FlagOperation.Add, false)))
                case ImapToken.Str("+FLAGS.SILENT") =>
                  CommandSuccess(Store(tag, set, StoreDataItem(flags, StoreDataItem.FlagOperation.Add, true)))
                case ImapToken.Str("-FLAGS") =>
                  CommandSuccess(Store(tag, set, StoreDataItem(flags, StoreDataItem.FlagOperation.Remove, false)))
                case ImapToken.Str("-FLAGS.SILENT") =>
                  CommandSuccess(Store(tag, set, StoreDataItem(flags, StoreDataItem.FlagOperation.Remove, true)))
                case _ => UnexpectedArguments(allTokens)
              }
            }
          case _ => UnexpectedArguments(allTokens)
        }
      }
    case "COPY" =>
      parameters.headOption.collect({
        case ImapToken.Str(string) => sequenceSetFromString(string) 
      }).flatten match {
        case None => UnexpectedArguments(allTokens)
        case Some(set) => parameters.lift(1) match {
          case Some(ImapToken.Str(string)) => CommandSuccess(Copy(tag, set, string))
          case _ => UnexpectedArguments(allTokens)
        }
      }
    case "UID" => parameters.lift(1) match {
      case Some(ImapToken.Str(subCmdName)) => parseCommand(tag, subCmdName, parameters.drop(2), allTokens) match {
        case fail: Failure => fail
        case CommandSuccess(cmd: Copy) => CommandSuccess(Uid(tag, UidCommand.Copy(cmd)))
        case CommandSuccess(cmd: Fetch) => CommandSuccess(Uid(tag, UidCommand.Fetch(cmd)))
        case CommandSuccess(cmd: Store) => CommandSuccess(Uid(tag, UidCommand.Store(cmd)))
        case CommandSuccess(cmd: Search) => CommandSuccess(Uid(tag, UidCommand.Search(cmd)))
        case _ => UnexpectedArguments(allTokens)
      }
      case _ => UnexpectedArguments(allTokens)
    }
    case cmdName if cmdName.startsWith("X") => CommandSuccess(Extension(tag, cmdName, parameters))
    case _ => UnrecognizedCommand(allTokens)
  }
  
  def fetchItemFromTokens(tokens: Seq[ImapToken]): Option[FetchItem] = tokens match {
    case Seq(ImapToken.Str("ALL")) => Some(Left(FetchMacro.All))
    case Seq(ImapToken.Str("FAST")) => Some(Left(FetchMacro.Fast))
    case Seq(ImapToken.Str("FULL")) => Some(Left(FetchMacro.Full))
    case Seq(ImapToken.List('(', tokens)) => fetchDataItemsFromTokens(tokens).map(Right(_))
    case _ => None
  }
  
  def fetchDataItemsFromTokens(tokens: Seq[ImapToken]): Option[Seq[FetchDataItem]] = {
    var currentTokens = tokens
    var items = Seq.empty[FetchDataItem]
    while (!currentTokens.isEmpty) {
      fetchDataItemFromTokens(currentTokens) match {
        case None => return None
        case Some((item, updatedTokens)) =>
          items :+= item
          currentTokens = updatedTokens
      }
    }
    Some(items)
  }
  
  def fetchDataItemFromTokens(tokens: Seq[ImapToken]): Option[(FetchDataItem, Seq[ImapToken])] = {
    if (tokens.isEmpty) return None
    tokens.head match {
      case ImapToken.Str("BODYSTRUCTURE") => Some(FetchDataItem.BodyStructure -> tokens.drop(1))
      case ImapToken.Str("ENVELOPE") => Some(FetchDataItem.Envelope -> tokens.drop(1))
      case ImapToken.Str("FLAGS") => Some(FetchDataItem.Flags -> tokens.drop(1))
      case ImapToken.Str("INTERNALDATE") => Some(FetchDataItem.InternalDate -> tokens.drop(1))
      case ImapToken.Str("RFC822") => Some(FetchDataItem.Rfc822 -> tokens.drop(1))
      case ImapToken.Str("RFC822.HEADER") => Some(FetchDataItem.Rfc822Header -> tokens.drop(1))
      case ImapToken.Str("RFC822.SIZE") => Some(FetchDataItem.Rfc822Size -> tokens.drop(1))
      case ImapToken.Str("RFC822.TEXT") => Some(FetchDataItem.Rfc822Text -> tokens.drop(1))
      case ImapToken.Str("UID") => Some(FetchDataItem.Uid -> tokens.drop(1))
      case ImapToken.Str("BODY") => tokens.lift(1) match {
        case Some(ImapToken.List('[', bodyTokens)) => fetchBodyPartsFromTokens(bodyTokens).flatMap { parts =>
          tokens.lift(2) match {
            case Some(ImapToken.Str(offsets)) => fetchOffsetsFromString(offsets) match {
              case None => Some(FetchDataItem.Body(parts) -> tokens.drop(2))
              case Some((offset, count)) =>
                Some(FetchDataItem.Body(parts, Some(offset), Some(count)) -> tokens.drop(3))
            }
            case _ => Some(FetchDataItem.Body(parts) -> tokens.drop(2))
          }
        }
        case _ => Some(FetchDataItem.NonExtensibleBodyStructure -> tokens.drop(1))
      }
      case ImapToken.Str("BODY.PEEK") => tokens.lift(1) match {
        case Some(ImapToken.List('[', bodyTokens)) => fetchBodyPartsFromTokens(bodyTokens).flatMap { parts =>
          tokens.lift(2) match {
            case Some(ImapToken.Str(offsets)) => fetchOffsetsFromString(offsets) match {
              case None => Some(FetchDataItem.BodyPeek(parts) -> tokens.drop(2))
              case Some((offset, count)) =>
                Some(FetchDataItem.BodyPeek(parts, Some(offset), Some(count)) -> tokens.drop(3))
            }
            case _ => Some(FetchDataItem.BodyPeek(parts) -> tokens.drop(2))
          }
        }
        case _ => Some(FetchDataItem.BodyPeek(Seq.empty) -> tokens.drop(1))
      }
      case _ => None
    }
  }
  
  def fetchOffsetsFromString(string: String): Option[(Int, Int)] = {
    if (!string.startsWith("<") || !string.endsWith(">")) return None
    val pieces = string.substring(1, string.length - 1).split('.')
    if (pieces.length != 2) None
    else Try(pieces(0).toInt).toOption.flatMap(i => Try(pieces(1).toInt).toOption.map(i -> _))
  }
  
  def fetchBodyPartsFromTokens(tokens: Seq[ImapToken]): Option[Seq[Imap.BodyPart]] = {
    if (tokens.isEmpty) return Some(Seq.empty)
    else if (!tokens.head.isInstanceOf[ImapToken.Str]) return None
    val ImapToken.Str(partName) = tokens.head
    val parts = partName.split('.').foldLeft(Option(Seq.empty[Imap.BodyPart])) {
      case (None, _) => None
      case (Some(seq), partPiece) => partPiece match {
        case "HEADER" => seq.lastOption match {
          case Some(_: Imap.BodyPart.Number) | None => Some(seq :+ Imap.BodyPart.Header)
          case _ => None
        } 
        case "FIELDS" => seq.lastOption match {
          case Some(Imap.BodyPart.Header) =>
            Some(seq.dropRight(1) :+ Imap.BodyPart.HeaderFields(Seq.empty))
          case _ => None
        }
        case "NOT" => seq.lastOption match {
          case Some(_: Imap.BodyPart.HeaderFields) =>
            Some(seq.dropRight(1) :+ Imap.BodyPart.HeaderFieldsNot(Seq.empty))
          case _ => None
        }
        case "TEXT" => seq.lastOption match {
          case Some(_: Imap.BodyPart.Number) | None => Some(seq :+ Imap.BodyPart.Text)
          case _ => None
        }
        case "MIME" => seq.lastOption match {
          case Some(_: Imap.BodyPart.Number) => Some(seq :+ Imap.BodyPart.Mime)
          case _ => None
        }
        case str => Try(str.toInt).toOption.flatMap { num =>
          seq.lastOption match {
            case Some(_: Imap.BodyPart.Number) | None => Some(seq :+ Imap.BodyPart.Number(num))
            case _ => None
          }
        }
      }
    }
    // Some parts require an extra list, some don't
    parts.flatMap(_.lastOption) match {
      case Some(_: Imap.BodyPart.HeaderFields) =>
        if (tokens.length != 2) None
        else headerFieldsFromToken(tokens(1)).flatMap { seq =>
          Some(parts.get.dropRight(1) :+ Imap.BodyPart.HeaderFields(seq))
        }
      case Some(_: Imap.BodyPart.HeaderFieldsNot) =>
        if (tokens.length != 2) None
        else headerFieldsFromToken(tokens(1)).flatMap { seq =>
          Some(parts.get.dropRight(1) :+ Imap.BodyPart.HeaderFieldsNot(seq))
        }
      case _  if (tokens.length == 1) => parts
      case _ => None
    }
  }
  
  def headerFieldsFromToken(token: ImapToken): Option[Seq[String]] = token match {
    case ImapToken.List('(', tokens) => tokens.foldLeft(Option(Seq.empty[String])) {
      case (seq, token) => seq.flatMap { seq =>
        token match {
          case ImapToken.Str(string) => Some(seq :+ string)
          case _ => None
        }
      }
    }
    case _ => None
  }
    
  def flagFromToken(flag: ImapToken): Option[Imap.Flag] = flag match {
    case ImapToken.Str("\\Seen") => Some(Imap.Flag.Seen)
    case ImapToken.Str("\\Answered") => Some(Imap.Flag.Answered)
    case ImapToken.Str("\\Flagged") => Some(Imap.Flag.Flagged)
    case ImapToken.Str("\\Deleted") => Some(Imap.Flag.Deleted)
    case ImapToken.Str("\\Draft") => Some(Imap.Flag.Draft)
    case ImapToken.Str("\\Recent") => Some(Imap.Flag.Recent)
    case ImapToken.Str(str) if str.startsWith("X") => Some(Imap.Flag.Extension(str))
    case ImapToken.Str(str) => Some(Imap.Flag.Keyword(str))
    case _ => None
  }
  
  def searchCriterias(tokens: Seq[ImapToken]): Option[Seq[SearchCriteria]] = {
    var currentTokens = tokens
    var resultCriteria = Seq.empty[SearchCriteria]
    while (!currentTokens.isEmpty) {
      searchCriteria(currentTokens) match {
        case None => return None
        case Some((crit, updatedTokens)) =>
          resultCriteria :+= crit
          currentTokens = updatedTokens
      }
    }
    Some(resultCriteria)
  }
  
  def searchCriteria(tokens: Seq[ImapToken]): Option[(SearchCriteria, Seq[ImapToken])] = {
    tokens.headOption.collect({ case ImapToken.Str(str) => str }).flatMap {
      case "ALL" => Some(SearchCriteria.All -> tokens.drop(1))
      case "ANSWERED" => Some(SearchCriteria.Answered -> tokens.drop(1))
      case "BCC" => tokens.lift(1).collect {
        case ImapToken.Str(string) => SearchCriteria.Bcc(string) -> tokens.drop(2)
      }
      case "BEFORE" => tokens.lift(1).collect({
        case ImapToken.Str(string) => Try(TokenSetToClientCommand.dateFormat.parseLocalDate(string)).map({ d =>
          SearchCriteria.Before(d) -> tokens.drop(2)
        }).toOption
      }).flatten
      case "BODY" => tokens.lift(1).collect {
        case ImapToken.Str(string) => SearchCriteria.Body(string) -> tokens.drop(2)
      }
      case "CC" => tokens.lift(1).collect {
        case ImapToken.Str(string) => SearchCriteria.Cc(string) -> tokens.drop(2)
      }
      case "DELETED" => Some(SearchCriteria.Deleted -> tokens.drop(1))
      case "DRAFT" => Some(SearchCriteria.Draft -> tokens.drop(1))
      case "FLAGGED" => Some(SearchCriteria.Flagged -> tokens.drop(1))
      case "FROM" => tokens.lift(1).collect {
        case ImapToken.Str(string) => SearchCriteria.From(string) -> tokens.drop(2)
      }
      case "HEADER" => tokens.lift(1).flatMap(h => tokens.lift(2).map(h -> _)).collect {
        case (ImapToken.Str(fieldName), ImapToken.Str(string)) =>
          SearchCriteria.Header(fieldName, string) -> tokens.drop(3)
      }
      case "KEYWORD" => tokens.lift(1).flatMap(flagFromToken(_).map(SearchCriteria.Keyword(_) -> tokens.drop(2)))
      case "LARGER" => tokens.lift(1).collect {
        case ImapToken.Str(string) => Try(string.toLong).toOption.map(SearchCriteria.Larger(_) -> tokens.drop(2))
      }.flatten
      case "NEW" => Some(SearchCriteria.New -> tokens.drop(1))
      case "NOT" => searchCriteria(tokens.drop(1)).map {
        case (crit, updatedTokens) => SearchCriteria.Not(crit) -> updatedTokens
      }
      case "OLD" => Some(SearchCriteria.New -> tokens.drop(1))
      case "ON" => tokens.lift(1).collect({
        case ImapToken.Str(string) => Try(TokenSetToClientCommand.dateFormat.parseLocalDate(string)).map({ d =>
          SearchCriteria.On(d) -> tokens.drop(2)
        }).toOption
      }).flatten
      case "OR" => searchCriteria(tokens.drop(1)).flatMap {
        case (lhs, updatedTokens) => searchCriteria(updatedTokens.drop(1)).map {
          case (rhs, updatedTokens) => SearchCriteria.Or(lhs, rhs) -> updatedTokens
        }
      }
      case "RECENT" => Some(SearchCriteria.Recent -> tokens.drop(1))
      case "SEEN" => Some(SearchCriteria.Seen -> tokens.drop(1))
      case "SENTBEFORE" => tokens.lift(1).collect({
        case ImapToken.Str(string) => Try(TokenSetToClientCommand.dateFormat.parseLocalDate(string)).map({ d =>
          SearchCriteria.SentBefore(d) -> tokens.drop(2)
        }).toOption
      }).flatten
      case "SENTON" => tokens.lift(1).collect({
        case ImapToken.Str(string) => Try(TokenSetToClientCommand.dateFormat.parseLocalDate(string)).map({ d =>
          SearchCriteria.SentOn(d) -> tokens.drop(2)
        }).toOption
      }).flatten
      case "SENTSINCE" => tokens.lift(1).collect({
        case ImapToken.Str(string) => Try(TokenSetToClientCommand.dateFormat.parseLocalDate(string)).map({ d =>
          SearchCriteria.SentSince(d) -> tokens.drop(2)
        }).toOption
      }).flatten
      case "SINCE" => tokens.lift(1).collect({
        case ImapToken.Str(string) => Try(TokenSetToClientCommand.dateFormat.parseLocalDate(string)).map({ d =>
          SearchCriteria.Since(d) -> tokens.drop(2)
        }).toOption
      }).flatten
      case "SMALLER" => tokens.lift(1).collect {
        case ImapToken.Str(string) => Try(string.toLong).toOption.map(SearchCriteria.Smaller(_) -> tokens.drop(2))
      }.flatten
      case "SUBJECT" => tokens.lift(1).collect {
        case ImapToken.Str(string) => SearchCriteria.Subject(string) -> tokens.drop(2)
      }
      case "TEXT" => tokens.lift(1).collect {
        case ImapToken.Str(string) => SearchCriteria.Text(string) -> tokens.drop(2)
      }
      case "TO" => tokens.lift(1).collect {
        case ImapToken.Str(string) => SearchCriteria.To(string) -> tokens.drop(2)
      }
      case "UID" => tokens.lift(1).collect({
        case ImapToken.Str(string) => sequenceSetFromString(string).map(SearchCriteria.Uid(_) -> tokens.drop(2))
      }).flatten
      case "UNANSWERED" => Some(SearchCriteria.Seen -> tokens.drop(1))
      case "UNDELETED" => Some(SearchCriteria.Seen -> tokens.drop(1))
      case "UNDRAFT" => Some(SearchCriteria.Seen -> tokens.drop(1))
      case "UNFLAGGED" => Some(SearchCriteria.Seen -> tokens.drop(1))
      case "UNKEYWORD" => tokens.lift(1).flatMap(flagFromToken(_).map(SearchCriteria.Unkeyword(_) -> tokens.drop(2)))
      case "UNSEEN" => Some(SearchCriteria.Seen -> tokens.drop(1))
      case str => sequenceSetFromString(str).map(SearchCriteria.SequenceSet(_) -> tokens.drop(2))
    }
  }
  
  def sequenceNumberFromString(string: String): Option[Imap.SequenceNumber] = {
    if (string == "*") Some(Imap.SequenceNumberAll)
    else Try(BigInt(string)).toOption.map(Imap.SequenceNumberLiteral)
  }
  
  def sequenceRangeFromString(string: String): Option[Imap.SequenceRange] = string.indexOf(':') match {
    case -1 => None
    case idx =>
      val (lhs, rhs) = string.splitAt(idx)
      sequenceNumberFromString(lhs).flatMap(lhs => sequenceNumberFromString(rhs).map(Imap.SequenceRange(lhs, _)))
  }
  
  def sequenceSetItemFromString(string: String): Option[Imap.SequenceSetItem] = {
    sequenceRangeFromString(string).orElse(sequenceNumberFromString(string))
  }
  
  def sequenceSetFromString(string: String): Option[Imap.SequenceSet] = {
    string.split(',').foldLeft(Option(Imap.SequenceSet(Seq.empty))) {
      case (Some(set), piece) => sequenceSetItemFromString(piece).map(i => set.copy(set.items :+ i))
      case (res, _) => res
    }
  }
}

object TokenSetToClientCommand {
  //DQUOTE date-day-fixed "-" date-month "-" date-year SP time SP zone DQUOTE
  val dateTimeFormat = DateTimeFormat.forPattern("dd-MMM-yyyy HH:mm:ss Z")
  //date-day "-" date-month "-" date-year
  val dateFormat = DateTimeFormat.forPattern("dd-MMM-yyyy")
  
  class Stage extends StatefulStage[Seq[ImapToken], ClientCommand.ParseResult] with TokenSetToClientCommand {
    override def initial = new State {
      override def onPush(chunk: Seq[ImapToken], ctx: Context[ClientCommand.ParseResult]): SyncDirective = {
        emit(Iterator.single(getClientCommand(chunk)), ctx)
      }
    }
  }
}