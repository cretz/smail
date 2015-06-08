package scimap
package handler

trait ServerHandler {
  def handle(res: Option[ClientCommand.ParseResult]): Seq[ServerResponse]
  
  def onInternalError(t: Throwable): Seq[ServerResponse] = {
    println("Failure: ", t, t.getStackTrace().mkString("\n\t", "\n\t", ""))
    failAndClose("Internal error")
  }
  
  def failAndClose(reason: String): Seq[ServerResponse] = {
    Seq(ServerResponse.Bye(reason), ServerResponse.CloseConnection)
  }
}