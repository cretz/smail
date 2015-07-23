package smail.smtp

sealed trait ServerResponse
object ServerResponse {
  
  case object CloseConnection extends ServerResponse
  
  case class Normal(
    code: Smtp.ReplyCode,
    text: String,
    asKeywordParam: Boolean = false
  ) extends ServerResponse
  object Normal {
    def apply(code: Smtp.ReplyCode): Normal = Normal(code, code.defaultString)
    def apply(code: Smtp.ReplyCode, asKeywordParam: Boolean): Normal = Normal(code, code.defaultString, asKeywordParam)
  }
}