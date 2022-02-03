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
import akka.Done

object Subscriptions {
  case class Feeds(since: String)
}
class Subscriptions extends Actor {
  
  var db: List[FeedAggregatorServer.FeedInfo] = List()
  implicit val ec = context.system.dispatcher
  implicit val timeOut = Timeout(20 seconds)
  val filterA = context.system.actorOf(Props[FilterA],"FilterA")

  def receive = {

    case Aggregator.Insert(feed) => {
      db = db ::: (List(feed))
      sender() ! Handler.OK
    }

    case Subscriptions.Feeds(since) => {
      var flag = 0
      var result: List[FeedAggregatorServer.FeedInfo] = List()
      val handler = sender()

      since match {
        case "0" => handler ! db
        case _ => {
          db.foreach{ feed =>
            (filterA ? FilterA.Getfilter(feed, since)).onComplete {
              case Success(value) => value match {
                case value: FeedAggregatorServer.FeedInfo => {
                  result = result ::: List(value)
                }
                case Handler.Bad(msg) => flag = 1
              }
              case Failure(value) => flag = 1 
            }
          }
          if (flag == 0) {
            handler ! result
          } else {
            handler ! Handler.Bad("Bad Format for since parameter")
          }
        }
      }
    }
  }
}

/* Ver que esta pasando que cuando se agrega un nuevo feed a la base
  de datos, la lista queda vacia. Y cada vez que llamo al endpoint
  feeds me devuelve []. */