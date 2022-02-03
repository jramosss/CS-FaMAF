package feedaggregator.actors

import akka.actor.{Actor, Status}
import scala.concurrent.Await
import scala.concurrent.duration._
import feedaggregator.actors.SyncA._

// This actor have the function of parsing the url to obtain the xml.Elem

object SyncA {
  case class Sync(url: String) extends RequestMsg
  case class SyncResponse(elem: xml.Elem) extends ResponseMsg

  def syncRequest(path: String): Either[Throwable, xml.Elem] = {
    import dispatch._, Defaults._
    val rss = 
      dispatch.Http.default(dispatch.url(path) OK dispatch.as.xml.Elem).either
    Await.result(rss, 15.seconds)
  }
}

class SyncA extends Actor {
    def receive = {
      case Sync(url) => {
        
        SyncA.syncRequest(url) match {
          case Right(value) => sender() ! Status.Success(SyncResponse(value))

          case Left(value) => sender() ! Status.Failure(value) 
        } 
      }
    }
}