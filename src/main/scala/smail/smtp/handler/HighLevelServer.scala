package smail.smtp
package handler

import scala.concurrent.Future

trait HighLevelServer {
  import HighLevelServer._
  
  def description(): Future[Either[String, Smtp.Failure]]
  def greeting(clientAddress: Option[String]): Future[Either[Greeting, Smtp.Failure]]
  def extensions(): Future[Either[Seq[String], Smtp.Failure]]
  def beginMail(from: Option[Smtp.Address], params: Seq[(String, String)]): Future[Option[Smtp.Failure]]
  def addPostmasterRecipient(domain: Option[String], params: Seq[(String, String)]): Future[Option[Smtp.Failure]]
  def addUserRecipient(addr: Smtp.Address, params: Seq[(String, String)]): Future[Option[Smtp.Failure]]
  def beginData(): Future[Option[Smtp.Failure]]
  def addDataLine(line: String): Future[Option[Smtp.Failure]]
  def endData(): Future[Option[Smtp.Failure]]
  def endMail(): Future[Option[Smtp.Failure]]
}
object HighLevelServer {
  case class Greeting(domain: String, greeting: Option[String] = None) {
    override def toString(): String = greeting.map(domain + " " + _).getOrElse(domain)
  }
}