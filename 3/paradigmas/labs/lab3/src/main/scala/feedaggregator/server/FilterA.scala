package feedaggregator.server

import akka.pattern.{ask, pipe} //
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

import java.text.{SimpleDateFormat, ParseException}
import akka.http.scaladsl.settings.ParserSettings.ErrorLoggingVerbosity.Simple
import java.{util => ju}
import akka.protobufv3.internal.TextFormat.ParseException

object FilterA {
  case class Getfilter(feedInfo: FeedAggregatorServer.FeedInfo, since: String)
}

class FilterA extends Actor {



  def receive = {
    case FilterA.Getfilter(feedInfo, since) => {
      var backhome = sender()    
      val date: SimpleDateFormat = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", ju.Locale.ENGLISH)
      val parm: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss", ju.Locale.ENGLISH)

      try {
        var sc = parm.parse(since)
        var par = None

        val result: FeedAggregatorServer.FeedInfo =
          FeedAggregatorServer.FeedInfo(
            feedInfo.title,
            feedInfo.description,
            feedInfo.items.filter(item => date.parse(item.pubDate)
            .compareTo(sc) > 0)
          )

        backhome ! result 
      } catch {
        case e: java.text.ParseException => {
          backhome ! Handler.Bad("Bad Format for since parameter")
        }
      }
    }
  }
}