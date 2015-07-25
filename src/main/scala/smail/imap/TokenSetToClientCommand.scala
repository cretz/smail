package smail.imap

import scala.util.Try
import java.time.ZonedDateTime
import java.time.LocalDate
import smail.Util

trait TokenSetToClientCommand extends (Seq[ImapToken] => ClientCommand.ParseResult) {
  import ClientCommand._
  import Util.Implicits._
  
  var waitingForText = Seq.empty[ImapToken]
  
  def apply(chunk: Seq[ImapToken]): ParseResult = getClientCommand(chunk)
  
  def getClientCommand(chunk: Seq[ImapToken]): ParseResult = {
    var properChunk = waitingForText ++ chunk
    waitingForText = Seq.empty
    properChunk.headOption match {
      case Some(ImapToken.Str(tag, _)) => properChunk.last match {
        case _: ImapToken.StrCountPrefix =>
          waitingForText = properChunk.dropRight(1)
          WaitingForMoreText(properChunk)
        case _ => properChunk.lift(1) match {
          // The next one has to be a string too
          case Some(ImapToken.Str(cmdName, _)) =>
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
    case ci"CAPABILITY" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(Capability(tag))
    case ci"NOOP" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(Noop(tag))
    case ci"LOGOUT" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(Logout(tag))
    case ci"STARTTLS" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(StartTls(tag))
    case ci"AUTHENTICATE" => parameters match {
      case Seq(ImapToken.Str(mechanism, _)) => CommandSuccess(Authenticate(tag, mechanism))
      case _ => UnexpectedArguments(allTokens)
    }
    case ci"LOGIN" => parameters match {
      case Seq(ImapToken.Str(username, _), ImapToken.Str(password, _)) =>
        CommandSuccess(Login(tag, username, password))
      case _ =>
        UnexpectedArguments(allTokens)
    }
    case ci"SELECT" => parameters match {
      case Seq(ImapToken.Str(mailbox, _)) => CommandSuccess(Select(tag, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case ci"EXAMINE" => parameters match {
      case Seq(ImapToken.Str(mailbox, _)) => CommandSuccess(Examine(tag, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case ci"CREATE" => parameters match {
      case Seq(ImapToken.Str(mailbox, _)) => CommandSuccess(Create(tag, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case ci"DELETE" => parameters match {
      case Seq(ImapToken.Str(mailbox, _)) => CommandSuccess(Delete(tag, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case ci"RENAME" => parameters match {
      case Seq(ImapToken.Str(existingMailbox, _), ImapToken.Str(newMailbox, _)) =>
        CommandSuccess(Rename(tag, existingMailbox, newMailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case ci"SUBSCRIBE" => parameters match {
      case Seq(ImapToken.Str(mailbox, _)) => CommandSuccess(Subscribe(tag, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case ci"UNSUBSCRIBE" => parameters match {
      case Seq(ImapToken.Str(mailbox, _)) => CommandSuccess(Unsubscribe(tag, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case ci"LIST" => parameters match {
      case Seq(ImapToken.Str(reference, _), ImapToken.Str(mailbox, _)) => CommandSuccess(List(tag, reference, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case ci"LSUB" => parameters match {
      case Seq(ImapToken.Str(reference, _), ImapToken.Str(mailbox, _)) => CommandSuccess(LSub(tag, reference, mailbox))
      case _ => UnexpectedArguments(allTokens)
    }
    case ci"STATUS" => parameters match {
      case Seq(ImapToken.Str(mailbox, _), ImapToken.List('(', dataItems)) =>
        dataItems.foldLeft(CommandSuccess(Status(tag, mailbox, Seq.empty)): ParseResult) {
          case (CommandSuccess(s: Status), ImapToken.Str(dataItem, _)) => dataItem match {
            case ci"MESSAGES" => CommandSuccess(s.copy(dataItems = s.dataItems :+ Imap.StatusDataItem.Messages))
            case ci"RECENT" => CommandSuccess(s.copy(dataItems = s.dataItems :+ Imap.StatusDataItem.Recent))
            case ci"UIDNEXT" => CommandSuccess(s.copy(dataItems = s.dataItems :+ Imap.StatusDataItem.UidNext))
            case ci"UIDVALIDITY" => CommandSuccess(s.copy(dataItems = s.dataItems :+ Imap.StatusDataItem.UidValidity))
            case ci"UNSEEN" => CommandSuccess(s.copy(dataItems = s.dataItems :+ Imap.StatusDataItem.Unseen))
            case _ => UnexpectedArguments(allTokens)
          }
          case (res, _) => res
        }
      case _ => UnexpectedArguments(allTokens)
    }
    case ci"APPEND" =>
      val pieces = parameters match {
        case Seq(ImapToken.Str(mailbox, _), ImapToken.List('(', flags),
          ImapToken.Str(dateTime, _), ImapToken.Str(message, _)) => Some((mailbox, flags, Some(dateTime), message))
        case Seq(ImapToken.Str(mailbox, _), ImapToken.List('(', flags), ImapToken.Str(message, _)) =>
          Some((mailbox, flags, None, message))
        case Seq(ImapToken.Str(mailbox, _), ImapToken.Str(dateTime, _), ImapToken.Str(message, _)) =>
          Some((mailbox, Seq.empty, Some(dateTime), message))
        case Seq(ImapToken.Str(mailbox, _), ImapToken.Str(message, _)) => Some((mailbox, Seq.empty, None, message))
        case _ => None
      }
      pieces match {
        case None => UnexpectedArguments(allTokens)
        case Some((mailbox, flags, dateTime, message)) =>
          flags.foldLeft(Option(Seq.empty[Imap.Flag])) {
            case (None, _) => None
            case (Some(seq), flag) => flagFromToken(flag).map(seq :+ _)
          } match {
            case Some(flags) => Try(dateTime.map(ZonedDateTime.parse(_, Imap.dateTimeFormat))) match {
              case scala.util.Success(dateTime) => CommandSuccess(Append(tag, mailbox, message, flags, dateTime))
              case _ => UnexpectedArguments(allTokens)
            }
            case None => UnexpectedArguments(allTokens)
          }
      }
    case ci"CHECK" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(Check(tag))
    case ci"CLOSE" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(Close(tag))
    case ci"EXPUNGE" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(Expunge(tag))
    case ci"SEARCH" =>
      val (charset, searchParameters) = parameters.take(2) match {
        case Seq(ImapToken.Str(ci"CHARSET", _), ImapToken.Str(charset, _)) => Some(charset) -> parameters.drop(2)
        case _ => None -> parameters
      }
      searchCriterias(searchParameters) match {
        case Some(crits) => CommandSuccess(Search(tag, crits, charset))
        case None => UnexpectedArguments(allTokens)
      }
    case ci"FETCH" =>
      parameters.headOption.collect({
        case ImapToken.Str(string, _) => sequenceSetFromString(string) 
      }).flatten match {
        case None => UnexpectedArguments(allTokens)
        case Some(set) => fetchItemFromTokens(parameters.drop(1)) match {
          case None => UnexpectedArguments(allTokens)
          case Some(item) => CommandSuccess(Fetch(tag, set, item))
        }
      }
    // TODO: here and elsewhere, let's check that there aren't extra params
    case ci"STORE" =>
      parameters.headOption.collect({
        case ImapToken.Str(string, _) => sequenceSetFromString(string) 
      }).flatten match {
        case None => UnexpectedArguments(allTokens)
        case Some(set) =>
          // Per the dovecot "store" IMAP tests, flags don't have to be in a parenthesized list
          val flagList = parameters.lift(2).flatMap {
            case ImapToken.List('(', flagList) => Some(flagList)
            case token: ImapToken.Str => Some(Seq(token))
            case _ => None
          }
          flagList.map({ flagList =>
            flagList.foldLeft(Option(Seq.empty[Imap.Flag])) {
              case (None, _) => None
              case (Some(seq), flag) => flagFromToken(flag).map(seq :+ _)
            } match {
              case None => UnexpectedArguments(allTokens)
              case Some(flags) => parameters(1) match {
                case ImapToken.Str(ci"FLAGS", _) =>
                  CommandSuccess(Store(tag, set, StoreDataItem(flags, Imap.FlagOperation.Replace, false)))
                case ImapToken.Str(ci"FLAGS.SILENT", _) =>
                  CommandSuccess(Store(tag, set, StoreDataItem(flags, Imap.FlagOperation.Replace, true)))
                case ImapToken.Str(ci"+FLAGS", _) =>
                  CommandSuccess(Store(tag, set, StoreDataItem(flags, Imap.FlagOperation.Add, false)))
                case ImapToken.Str(ci"+FLAGS.SILENT", _) =>
                  CommandSuccess(Store(tag, set, StoreDataItem(flags, Imap.FlagOperation.Add, true)))
                case ImapToken.Str(ci"-FLAGS", _) =>
                  CommandSuccess(Store(tag, set, StoreDataItem(flags, Imap.FlagOperation.Remove, false)))
                case ImapToken.Str(ci"-FLAGS.SILENT", _) =>
                  CommandSuccess(Store(tag, set, StoreDataItem(flags, Imap.FlagOperation.Remove, true)))
                case _ => UnexpectedArguments(allTokens)
              }
            }
          }).getOrElse(UnexpectedArguments(allTokens))
      }
    case ci"COPY" =>
      parameters.headOption.collect({
        case ImapToken.Str(string, _) => sequenceSetFromString(string) 
      }).flatten match {
        case None => UnexpectedArguments(allTokens)
        case Some(set) => parameters.lift(1) match {
          case Some(ImapToken.Str(string, _)) => CommandSuccess(Copy(tag, set, string))
          case _ => UnexpectedArguments(allTokens)
        }
      }
    case ci"UID" => parameters.lift(0) match {
      case Some(ImapToken.Str(subCmdName, _)) =>
        parseCommand(tag, subCmdName, parameters.drop(1), allTokens) match {
          case fail: Failure => fail
          case CommandSuccess(cmd: Copy) => CommandSuccess(Uid(tag, UidCommand.Copy(cmd)))
          case CommandSuccess(cmd: Fetch) => CommandSuccess(Uid(tag, UidCommand.Fetch(cmd)))
          case CommandSuccess(cmd: Store) => CommandSuccess(Uid(tag, UidCommand.Store(cmd)))
          case CommandSuccess(cmd: Search) => CommandSuccess(Uid(tag, UidCommand.Search(cmd)))
          case _ => UnexpectedArguments(allTokens)
        }
      case _ => UnexpectedArguments(allTokens)
    }
    case ci"IDLE" =>
      if (!parameters.isEmpty) UnexpectedArguments(allTokens)
      else CommandSuccess(Idle(tag))
    case cmdName if cmdName.startsWith("X") => CommandSuccess(Extension(tag, cmdName, parameters))
    case _ => UnrecognizedCommand(allTokens)
  }
  
  def fetchItemFromTokens(tokens: Seq[ImapToken]): Option[FetchItem] = tokens match {
    case Seq(ImapToken.Str(ci"ALL", _)) => Some(Left(FetchMacro.All))
    case Seq(ImapToken.Str(ci"FAST", _)) => Some(Left(FetchMacro.Fast))
    case Seq(ImapToken.Str(ci"FULL", _)) => Some(Left(FetchMacro.Full))
    case Seq(ImapToken.List('(', tokens)) => fetchDataItemsFromTokens(tokens).map(Right(_))
    case _ => fetchDataItemsFromTokens(tokens).map(Right(_))
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
      case ImapToken.Str(ci"BODYSTRUCTURE", _) => Some(FetchDataItem.BodyStructure -> tokens.drop(1))
      case ImapToken.Str(ci"ENVELOPE", _) => Some(FetchDataItem.Envelope -> tokens.drop(1))
      case ImapToken.Str(ci"FLAGS", _) => Some(FetchDataItem.Flags -> tokens.drop(1))
      case ImapToken.Str(ci"INTERNALDATE", _) => Some(FetchDataItem.InternalDate -> tokens.drop(1))
      case ImapToken.Str(ci"RFC822", _) => Some(FetchDataItem.Rfc822 -> tokens.drop(1))
      case ImapToken.Str(ci"RFC822.HEADER", _) => Some(FetchDataItem.Rfc822Header -> tokens.drop(1))
      case ImapToken.Str(ci"RFC822.SIZE", _) => Some(FetchDataItem.Rfc822Size -> tokens.drop(1))
      case ImapToken.Str(ci"RFC822.TEXT", _) => Some(FetchDataItem.Rfc822Text -> tokens.drop(1))
      case ImapToken.Str(ci"UID", _) => Some(FetchDataItem.Uid -> tokens.drop(1))
      case ImapToken.Str(ci"BODY", _) => tokens.lift(1) match {
        case Some(ImapToken.List('[', bodyTokens)) => fetchBodyPartFromTokens(bodyTokens).flatMap { part =>
          tokens.lift(2) match {
            case Some(ImapToken.Str(offsets, _)) => fetchOffsetsFromString(offsets) match {
              case None => Some(FetchDataItem.Body(part) -> tokens.drop(2))
              case Some((offset, count)) =>
                Some(FetchDataItem.Body(part, Some(offset), Some(count)) -> tokens.drop(3))
            }
            case _ => Some(FetchDataItem.Body(part) -> tokens.drop(2))
          }
        }
        case _ => Some(FetchDataItem.NonExtensibleBodyStructure -> tokens.drop(1))
      }
      case ImapToken.Str(ci"BODY.PEEK", _) => tokens.lift(1) match {
        case Some(ImapToken.List('[', bodyTokens)) => fetchBodyPartFromTokens(bodyTokens).flatMap { part =>
          tokens.lift(2) match {
            case Some(ImapToken.Str(offsets, _)) => fetchOffsetsFromString(offsets) match {
              case None => Some(FetchDataItem.BodyPeek(part) -> tokens.drop(2))
              case Some((offset, count)) =>
                Some(FetchDataItem.BodyPeek(part, Some(offset), Some(count)) -> tokens.drop(3))
            }
            case _ => Some(FetchDataItem.BodyPeek(part) -> tokens.drop(2))
          }
        }
        case _ => Some(FetchDataItem.BodyPeek(Imap.BodyPart.Part(Seq.empty)) -> tokens.drop(1))
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
  
  def fetchBodyPartFromTokens(tokens: Seq[ImapToken]): Option[Imap.BodyPart] = {
    if (tokens.isEmpty) return Some(Imap.BodyPart.Part(Seq.empty))
    else if (!tokens.head.isInstanceOf[ImapToken.Str]) return None
    val ImapToken.Str(partName, _) = tokens.head
    val part = partName.split('.').foldLeft(Option(Imap.BodyPart.Part(Seq.empty): Imap.BodyPart)) {
      case (None, _) => None
      case (Some(Imap.BodyPart.Part(nums)), ci"HEADER") => Some(Imap.BodyPart.Header(nums))
      case (Some(Imap.BodyPart.Part(nums)), ci"TEXT") => Some(Imap.BodyPart.Text(nums))
      case (Some(Imap.BodyPart.Part(nums)), ci"MIME") => Some(Imap.BodyPart.Header(nums))
      case (Some(Imap.BodyPart.Header(nums)), ci"FIELDS") => Some(Imap.BodyPart.HeaderFields(nums, Seq.empty))
      case (Some(Imap.BodyPart.HeaderFields(nums, fields)), ci"NOT") => Some(Imap.BodyPart.HeaderFieldsNot(nums, Seq.empty))
      case (Some(Imap.BodyPart.Part(nums)), num) => Try(num.toInt).toOption.map(i => Imap.BodyPart.Part(nums :+ i))
      case _ => None
    }
    // Some parts require an extra list, some don't
    part.flatMap {
      case part: Imap.BodyPart.HeaderFields =>
        if (tokens.length != 2) None
        else headerFieldsFromToken(tokens(1)).map(s => part.copy(fields = s))
      case part: Imap.BodyPart.HeaderFieldsNot =>
        if (tokens.length != 2) None
        else headerFieldsFromToken(tokens(1)).map(s => part.copy(fields = s))
      case _  if tokens.length == 1 => part
      case _ => None
    }
  }
  
  def headerFieldsFromToken(token: ImapToken): Option[Seq[String]] = token match {
    case ImapToken.List('(', tokens) => tokens.foldLeft(Option(Seq.empty[String])) {
      case (seq, token) => seq.flatMap { seq =>
        token match {
          case ImapToken.Str(string, _) => Some(seq :+ string)
          case _ => None
        }
      }
    }
    case _ => None
  }
    
  def flagFromToken(flag: ImapToken): Option[Imap.Flag] = flag match {
    case ImapToken.Str(ci"\\Seen", _) => Some(Imap.Flag.Seen)
    case ImapToken.Str(ci"\\Answered", _) => Some(Imap.Flag.Answered)
    case ImapToken.Str(ci"\\Flagged", _) => Some(Imap.Flag.Flagged)
    case ImapToken.Str(ci"\\Deleted", _) => Some(Imap.Flag.Deleted)
    case ImapToken.Str(ci"\\Draft", _) => Some(Imap.Flag.Draft)
    case ImapToken.Str(ci"\\Recent", _) => Some(Imap.Flag.Recent)
    case ImapToken.Str(str, _) if str.startsWith("X") => Some(Imap.Flag.Extension(str))
    case ImapToken.Str(str, _) => Some(Imap.Flag.Keyword(str))
    case _ => None
  }
  
  def searchCriterias(tokens: Seq[ImapToken]): Option[Seq[Imap.SearchCriterion]] = {
    var currentTokens = tokens
    var resultCriteria = Seq.empty[Imap.SearchCriterion]
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
  
  def searchCriteria(tokens: Seq[ImapToken]): Option[(Imap.SearchCriterion, Seq[ImapToken])] = {
    tokens.headOption.collect({ case ImapToken.Str(str, _) => str }).flatMap {
      case ci"ALL" => Some(Imap.SearchCriterion.All -> tokens.drop(1))
      case ci"ANSWERED" => Some(Imap.SearchCriterion.Answered -> tokens.drop(1))
      case ci"BCC" => tokens.lift(1).collect {
        case ImapToken.Str(string, _) => Imap.SearchCriterion.Bcc(string) -> tokens.drop(2)
      }
      case ci"BEFORE" => tokens.lift(1).collect({
        case ImapToken.Str(string, _) => Try(LocalDate.parse(string, Imap.dateFormat)).map({ d =>
          Imap.SearchCriterion.Before(d) -> tokens.drop(2)
        }).toOption
      }).flatten
      case ci"BODY" => tokens.lift(1).collect {
        case ImapToken.Str(string, _) => Imap.SearchCriterion.Body(string) -> tokens.drop(2)
      }
      case ci"CC" => tokens.lift(1).collect {
        case ImapToken.Str(string, _) => Imap.SearchCriterion.Cc(string) -> tokens.drop(2)
      }
      case ci"DELETED" => Some(Imap.SearchCriterion.Deleted -> tokens.drop(1))
      case ci"DRAFT" => Some(Imap.SearchCriterion.Draft -> tokens.drop(1))
      case ci"FLAGGED" => Some(Imap.SearchCriterion.Flagged -> tokens.drop(1))
      case ci"FROM" => tokens.lift(1).collect {
        case ImapToken.Str(string, _) => Imap.SearchCriterion.From(string) -> tokens.drop(2)
      }
      case ci"HEADER" => tokens.lift(1).flatMap(h => tokens.lift(2).map(h -> _)).collect {
        case (ImapToken.Str(fieldName, _), ImapToken.Str(string, _)) =>
          Imap.SearchCriterion.Header(fieldName, string) -> tokens.drop(3)
      }
      case ci"KEYWORD" => tokens.lift(1).flatMap(flagFromToken(_).map(Imap.SearchCriterion.Keyword(_) -> tokens.drop(2)))
      case ci"LARGER" => tokens.lift(1).collect {
        case ImapToken.Str(string, _) => Try(string.toLong).toOption.map(Imap.SearchCriterion.Larger(_) -> tokens.drop(2))
      }.flatten
      case ci"NEW" => Some(Imap.SearchCriterion.New -> tokens.drop(1))
      case ci"NOT" => searchCriteria(tokens.drop(1)).map {
        case (crit, updatedTokens) => Imap.SearchCriterion.Not(crit) -> updatedTokens
      }
      case ci"OLD" => Some(Imap.SearchCriterion.New -> tokens.drop(1))
      case ci"ON" => tokens.lift(1).collect({
        case ImapToken.Str(string, _) => Try(LocalDate.parse(string, Imap.dateFormat)).map({ d =>
          Imap.SearchCriterion.On(d) -> tokens.drop(2)
        }).toOption
      }).flatten
      case ci"OR" => searchCriteria(tokens.drop(1)).flatMap {
        case (lhs, updatedTokens) => searchCriteria(updatedTokens).map {
          case (rhs, updatedTokens) => Imap.SearchCriterion.Or(lhs, rhs) -> updatedTokens
        }
      }
      case ci"RECENT" => Some(Imap.SearchCriterion.Recent -> tokens.drop(1))
      case ci"SEEN" => Some(Imap.SearchCriterion.Seen -> tokens.drop(1))
      case ci"SENTBEFORE" => tokens.lift(1).collect({
        case ImapToken.Str(string, _) => Try(LocalDate.parse(string, Imap.dateFormat)).map({ d =>
          Imap.SearchCriterion.SentBefore(d) -> tokens.drop(2)
        }).toOption
      }).flatten
      case ci"SENTON" => tokens.lift(1).collect({
        case ImapToken.Str(string, _) => Try(LocalDate.parse(string, Imap.dateFormat)).map({ d =>
          Imap.SearchCriterion.SentOn(d) -> tokens.drop(2)
        }).toOption
      }).flatten
      case ci"SENTSINCE" => tokens.lift(1).collect({
        case ImapToken.Str(string, _) => Try(LocalDate.parse(string, Imap.dateFormat)).map({ d =>
          Imap.SearchCriterion.SentSince(d) -> tokens.drop(2)
        }).toOption
      }).flatten
      case ci"SINCE" => tokens.lift(1).collect({
        case ImapToken.Str(string, _) => Try(LocalDate.parse(string, Imap.dateFormat)).map({ d =>
          Imap.SearchCriterion.Since(d) -> tokens.drop(2)
        }).toOption
      }).flatten
      case ci"SMALLER" => tokens.lift(1).collect {
        case ImapToken.Str(string, _) =>
          Try(string.toLong).toOption.map(Imap.SearchCriterion.Smaller(_) -> tokens.drop(2))
      }.flatten
      case ci"SUBJECT" => tokens.lift(1).collect {
        case ImapToken.Str(string, _) => Imap.SearchCriterion.Subject(string) -> tokens.drop(2)
      }
      case ci"TEXT" => tokens.lift(1).collect {
        case ImapToken.Str(string, _) => Imap.SearchCriterion.Text(string) -> tokens.drop(2)
      }
      case ci"TO" => tokens.lift(1).collect {
        case ImapToken.Str(string, _) => Imap.SearchCriterion.To(string) -> tokens.drop(2)
      }
      case ci"UID" => tokens.lift(1).collect({
        case ImapToken.Str(string, _) =>
          sequenceSetFromString(string).map(Imap.SearchCriterion.Uid(_) -> tokens.drop(2))
      }).flatten
      case ci"UNANSWERED" => Some(Imap.SearchCriterion.Seen -> tokens.drop(1))
      case ci"UNDELETED" => Some(Imap.SearchCriterion.Seen -> tokens.drop(1))
      case ci"UNDRAFT" => Some(Imap.SearchCriterion.Seen -> tokens.drop(1))
      case ci"UNFLAGGED" => Some(Imap.SearchCriterion.Seen -> tokens.drop(1))
      case ci"UNKEYWORD" =>
        tokens.lift(1).flatMap(flagFromToken(_).map(Imap.SearchCriterion.Unkeyword(_) -> tokens.drop(2)))
      case ci"UNSEEN" => Some(Imap.SearchCriterion.Seen -> tokens.drop(1))
      case str => sequenceSetFromString(str).map(Imap.SearchCriterion.SequenceSet(_) -> tokens.drop(2))
    }
  }
  
  def sequenceNumberFromString(string: String): Option[Imap.SequenceNumber] = {
    if (string == "*") Some(Imap.SequenceNumberAll)
    else Try(BigInt(string)).toOption.map(Imap.SequenceNumberLiteral)
  }
  
  def sequenceRangeFromString(string: String): Option[Imap.SequenceRange] = string.indexOf(':') match {
    case -1 => None
    case idx =>
      val lhs = string.take(idx)
      val rhs = string.drop(idx + 1)
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
  def apply(): TokenSetToClientCommand = new TokenSetToClientCommand() { }
}