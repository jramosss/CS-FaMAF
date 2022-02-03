package feedaggregator.server

import akka.actor.ActorSystem
import akka.actor.{ActorRef, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import scala.io.StdIn
import scala.concurrent.Await
import scala.concurrent.duration._

object FeedAggregatorServer {

  final case class FeedItem(title: String, link: String, description: Option[String], pubDate: String)
  final case class FeedInfo(title: String, description: Option[String], items: List[FeedItem])


  // Needed for Unmarshalling
  implicit val feedItem = jsonFormat4(FeedItem)
  implicit val feedInfo = jsonFormat3(FeedInfo)

  def main(args: Array[String]) {
    implicit val system = ActorSystem()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    // val subscriptions = system.actorOf(Props[Subscriptions], "Subscriptions")
    val handler = system.actorOf(Props[Handler], "Handler")

    handler ! Handler.Handle
  }

}