package smail.imap

import java.time.ZonedDateTime
import java.time.LocalDate

sealed trait ClientCommand {
  def tag: String
}

object ClientCommand {
  sealed trait ParseResult
  sealed trait Success extends ParseResult
  case class CommandSuccess(command: ClientCommand) extends Success
  case class WaitingForMoreText(tokens: Seq[ImapToken]) extends Success
  sealed trait Failure extends ParseResult {
    def tokens: Seq[ImapToken]
  }
  case class UnexpectedArguments(tokens: Seq[ImapToken]) extends Failure
  case class UnrecognizedCommand(tokens: Seq[ImapToken]) extends Failure
  case class UnknownFailure(message: String, tokens: Seq[ImapToken]) extends Failure
  
  sealed trait State
  sealed trait NotAuthenticatedState extends State
  sealed trait AuthenticatedState extends NotAuthenticatedState
  sealed trait SelectedState extends AuthenticatedState
  sealed trait AnyState extends SelectedState
  
  case class Capability(tag: String) extends ClientCommand with AnyState
  case class Noop(tag: String) extends ClientCommand with AnyState
  case class Logout(tag: String) extends ClientCommand with AnyState
  case class StartTls(tag: String) extends ClientCommand with NotAuthenticatedState
  case class Authenticate(tag: String, mechanism: String) extends ClientCommand with NotAuthenticatedState
  case class Login(tag: String, username: String, password: String) extends ClientCommand with NotAuthenticatedState
  case class Select(tag: String, mailbox: String) extends ClientCommand with AuthenticatedState
  case class Examine(tag: String, mailbox: String) extends ClientCommand with AuthenticatedState
  case class Create(tag: String, mailbox: String) extends ClientCommand with AuthenticatedState
  case class Delete(tag: String, mailbox: String) extends ClientCommand with AuthenticatedState
  case class Rename(
    tag: String,
    existingMailbox: String,
    newMailbox: String
  ) extends ClientCommand with AuthenticatedState
  case class Subscribe(tag: String, mailbox: String) extends ClientCommand with AuthenticatedState
  case class Unsubscribe(tag: String, mailbox: String) extends ClientCommand with AuthenticatedState
  case class List(tag: String, reference: String, mailbox: String) extends ClientCommand with AuthenticatedState
  case class LSub(tag: String, reference: String, mailbox: String) extends ClientCommand with AuthenticatedState
  
  case class Status(
    tag: String,
    mailbox: String,
    dataItems: Seq[Imap.StatusDataItem]
  ) extends ClientCommand with AuthenticatedState
  
  case class Append(
    tag: String,
    mailbox: String,
    message: String,
    flags: Seq[Imap.Flag] = Seq.empty,
    date: Option[ZonedDateTime] = None
  ) extends ClientCommand with AuthenticatedState
  
  case class Check(tag: String) extends ClientCommand with SelectedState
  case class Close(tag: String) extends ClientCommand with SelectedState
  case class Expunge(tag: String) extends ClientCommand with SelectedState
  
  case class Search(
    tag: String,
    criteria: Seq[Imap.SearchCriterion],
    charset: Option[String] = None
  ) extends ClientCommand with SelectedState
  
  type FetchItem = Either[FetchMacro, Seq[FetchDataItem]]
  case class Fetch(tag: String, set: Imap.SequenceSet, dataItems: FetchItem) extends ClientCommand with SelectedState
  
  sealed trait FetchMacro
  object FetchMacro {
    case object All extends FetchMacro
    case object Fast extends FetchMacro
    case object Full extends FetchMacro
  }
  
  sealed trait FetchDataItem
  object FetchDataItem {
    case object NonExtensibleBodyStructure extends FetchDataItem
    case class Body(
      part: Imap.BodyPart,
      octetOffset: Option[Int] = None,
      octetCount: Option[Int] = None
    ) extends FetchDataItem
    
    case class BodyPeek(
      part: Imap.BodyPart,
      octetOffset: Option[Int] = None,
      octetCount: Option[Int] = None
    ) extends FetchDataItem
    case object BodyStructure extends FetchDataItem
    case object Envelope extends FetchDataItem
    case object Flags extends FetchDataItem
    case object InternalDate extends FetchDataItem
    case object Rfc822 extends FetchDataItem
    case object Rfc822Header extends FetchDataItem
    case object Rfc822Size extends FetchDataItem
    case object Rfc822Text extends FetchDataItem
    case object Uid extends FetchDataItem
  }
  
  case class Store(
    tag: String,
    set: Imap.SequenceSet,
    dataItem: StoreDataItem
  ) extends ClientCommand with SelectedState
  case class StoreDataItem(flags: Seq[Imap.Flag], operation: Imap.FlagOperation, silent: Boolean)
  
  case class Copy(tag: String, set: Imap.SequenceSet, mailbox: String) extends ClientCommand with SelectedState
  
  case class Uid(tag: String, command: UidCommand) extends ClientCommand with SelectedState
  sealed trait UidCommand {
    def command: ClientCommand
  }
  object UidCommand {
    case class Copy(command: ClientCommand.Copy) extends UidCommand
    case class Fetch(command: ClientCommand.Fetch) extends UidCommand
    case class Store(command: ClientCommand.Store) extends UidCommand
    case class Search(command: ClientCommand.Search) extends UidCommand
  }
  
  case class Idle(tag: String) extends ClientCommand
  
  case class Extension(tag: String, name: String, arguments: Seq[ImapToken]) extends ClientCommand with AnyState
}