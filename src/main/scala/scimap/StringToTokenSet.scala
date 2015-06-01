package scimap

import akka.stream.stage.StatefulStage
import akka.stream.stage.Context
import akka.stream.stage.SyncDirective
import scala.annotation.tailrec

trait StringToTokenSet {
  var tokenBuffer = Seq.empty[ImapToken]
  val buffer = new StringBuilder
  val parser = new ImapTokenParser(new ImapTokenParser.BufferedParserInput(buffer))
  
  def appendString(str: String): Unit = {
    buffer.append(str)
  }
  
  def nextTokenSets(): Seq[Seq[ImapToken]] = {
    // Parse should never fail here
    tokenBuffer ++= parser.Tokens.run().get
    // Break on newlines or count prefixes
    val (sets, newBuffer) = StringToTokenSet.splitTokensOnNewlineOrCountPrefix(tokenBuffer)
    // Trim up the string to as far as we got
    // Note, if we ended with a count prefix, we must not pass it
    sets.headOption.flatMap(_.lastOption) match {
      case Some(_: ImapToken.StrCountPrefix) => buffer.delete(0, buffer.lastIndexOf('{'))
      case _ => buffer.delete(0, parser.cursor)
    }
    
    tokenBuffer = newBuffer
    sets
  }
}

object StringToTokenSet {
  def splitTokensOnNewlineOrCountPrefix(
    input: Seq[ImapToken],
    tokens: Seq[Seq[ImapToken]] = Seq.empty
  ): (Seq[Seq[ImapToken]], Seq[ImapToken]) = {
    var ret = tokens
    var leftover = Seq.empty[ImapToken]
    input.foreach {
      // With newline we don't add the token, but with count prefix we do
      case ImapToken.Newline =>
        if (!leftover.isEmpty) ret :+= leftover
        leftover = Seq.empty[ImapToken]
      case token: ImapToken.StrCountPrefix =>
        // Count prefix means we stop all processing right now because a regular
        //  Str would have happened if the string came with it
        return (ret :+ leftover) -> Seq.empty[ImapToken]
      case token => leftover :+= token
    }
    ret -> leftover
  }
  
  class Stage extends StatefulStage[String, Seq[ImapToken]] with StringToTokenSet {
    override def initial = new State {
      override def onPush(chunk: String, ctx: Context[Seq[ImapToken]]): SyncDirective = {
        appendString(chunk)
        emit(nextTokenSets().iterator, ctx)
      }
    }
  }
}