package scimap.smtp
package handler

import scala.concurrent.Future

trait ServerHandler {
  def handle(res: Option[ClientCommand]): Option[Future[Seq[ServerResponse]]]
}