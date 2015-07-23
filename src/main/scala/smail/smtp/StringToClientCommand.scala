package smail.smtp

class StringToClientCommand extends (String => Seq[ClientCommand]) {
  val buffer = new StringBuilder
  
  def apply(str: String): Seq[ClientCommand] = {
    buffer ++= str
    // Break it apart across 
    ???
  }
}