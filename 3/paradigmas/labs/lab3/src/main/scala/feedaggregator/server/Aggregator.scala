package feedaggregator.server

import akka.actor.{ActorContext, ActorSystem, Actor, ActorRef,
   ActorRefFactory, Props}
import akka.pattern.ask //
import akka.pattern.pipe //
import akka.util.Timeout
import akka.http.scaladsl.Http
import scala.language.postfixOps //
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import scala.io.StdIn
import scala.util.{Success, Failure}
import scala.concurrent.Await
import scala.concurrent.duration._
import akka.compat.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor
import feedaggregator.server.FeedAggregatorServer.FeedInfo
import akka.Done

object Aggregator {
  case class Insert(feed: FeedAggregatorServer.FeedInfo)
}

class Aggregator extends Actor {


  implicit val executionContext = context.system.dispatcher
  implicit val timeOut = Timeout(20 seconds)

  val afeed = context.actorOf(Props[FeedA], "FeedA")
  var status = ""
  // val subscriptions = context.actorOf(Props[Subscriptions], "Subscriptions")
  
  var result: FeedInfo = FeedInfo (
    "",Some(""),List()
    )
  
  def receive = {
    case Handler.Add(url, sb) => {

      val originsend = sender()
      (afeed ? FeedA.Parsear(url,"0"))
      .onComplete {
        case Success(value) => value match {
          case value:FeedAggregatorServer.FeedInfo => {
            (sb ? Aggregator.Insert(value))
            .onComplete {
              case Success(value) => value match {
                case Handler.OK =>
                  originsend ! Handler.OK
                case Handler.Bad(msg) => 
                  originsend ! Handler.Bad(msg)
              }
              case Failure(res) =>
                originsend ! Handler.Bad("400 - Bad Request")
            }
          }
          case Handler.Bad(msg) =>
            originsend ! Handler.Bad(msg)
        }
        case Failure(value) =>
          originsend ! Handler.Bad("404 - Bad Request: url not found")
      }
    }
  }
}