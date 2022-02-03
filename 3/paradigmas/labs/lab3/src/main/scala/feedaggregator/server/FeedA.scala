package feedaggregator.server

import akka.pattern.ask //
import akka.util.Timeout
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.actor.{ActorContext, ActorSystem, Actor, ActorRef,
  ActorRefFactory, Props}

  
import spray.json.DefaultJsonProtocol._
import scala.io.StdIn
import scala.util.{Success, Failure}
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutor

import java.text.SimpleDateFormat
import akka.protobufv3.internal.TextFormat.ParseException

object FeedA {

  case class Parsear(url: String, since:String)
   
  def syncRequest(path: String): Either[Throwable, xml.Elem] = {
    import dispatch._, Defaults._
    val rss = dispatch.Http.default(dispatch.url(path) OK dispatch.as.xml.Elem).either
    Await.result(rss, 15.seconds)
  }
} 


class FeedA extends Actor {
    
  implicit val excontext = context.system.dispatcher
  implicit val timeout = Timeout(20.seconds)

  val filterA = context.actorOf(Props[FilterA],"filterA")
  
  // Hay que ver como se parsea bien esto...
  def receive = {
    
    case FeedA.Parsear(url,since) => {
      val handlerA = sender()

      FeedA.syncRequest(url) match{
        case Right(feed) => {
          val feedInfo = FeedAggregatorServer.FeedInfo(
            ((feed \ "channel") \ "title").headOption.map(_.text).get,
            ((feed \ "channel") \ "description").headOption.map(_.text),
            ((feed \ "channel") \\ "item").map(item =>              
              FeedAggregatorServer.FeedItem(
                (item \ "title").headOption.map(_.text).get,
                (item \ "link").headOption.map(_.text).get,
                (item \ "description").headOption.map(_.text),
                (item \ "pubDate").headOption.map(_.text).get)
            ).toList
          )

          
            
          // Me fijo como me vino el since, si lo voy a filtrar o no.
          since match{
            case "0" => handlerA ! feedInfo 
            case _ => { 
              (filterA ? FilterA.Getfilter(feedInfo,since)).onComplete{
                case Success(value) =>  value match {
                  case value:FeedAggregatorServer.FeedInfo => handlerA ! value
                  case Handler.Bad(msg) => {
                    handlerA ! Handler.Bad(msg)
                  }
                }
                case Failure(value) => {
                  handlerA ! Handler.Bad("400 - Bad Request")
                }
              }
            }
          }    
        }
        case Left(feed) => {
          sender() ! Handler.Bad(s"404: ${url} not found")
        }
      }   
    }
  }  
}