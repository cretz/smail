package scimap

import scala.annotation.tailrec

trait StringToTokenSet extends (String => Seq[Seq[ImapToken]]) {
  var tokenBuffer = Seq.empty[ImapToken]
  val buffer = new StringBuilder
  
  def apply(str: String): Seq[Seq[ImapToken]] ={
    appendString(str)
    nextTokenSets()
  }
  
  def appendString(str: String): Unit = {
    buffer.append(str)
  }
  
  def nextTokenSets(): Seq[Seq[ImapToken]] = {
    // Parse should never fail here
    val parser = new ImapTokenParser(new ImapTokenParser.BufferedParserInput(buffer))
    val result = parser.Tokens.run()
    tokenBuffer ++= result.get
    
    // Break on newlines or count prefixes
    val (sets, newBuffer) = splitTokensOnNewlineOrCountPrefix(tokenBuffer)
    // Trim up the string to as far as we got
    // Note, if we ended with a count prefix, we must not pass it
    sets.headOption.flatMap(_.lastOption) match {
      case Some(_: ImapToken.StrCountPrefix) => buffer.delete(0, buffer.lastIndexOf('{'))
      case _ => buffer.delete(0, parser.cursor)
    }
    
    tokenBuffer = newBuffer
    sets
  }

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
        leftover :+= token
        return (ret :+ leftover) -> Seq.empty[ImapToken]
      case token => leftover :+= token
    }
    ret -> leftover
  }
}

object StringToTokenSet {
  def apply(): StringToTokenSet = new StringToTokenSet() { }
}