package feedaggregator.actors

import feedaggregator.server.Handler
import akka.pattern.ask
import akka.util.Timeout
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.actor.{ActorContext, ActorSystem, Actor, ActorRef,
  ActorRefFactory, Props, Status}
import scala.util.{Success, Failure}
import scala.concurrent.duration._
import java.text.{SimpleDateFormat, ParseException}
import java.{util => ju}
import akka.http.javadsl.model.headers.Date
import feedaggregator.actors.FilterA.FilterResponse

// This actor have the function of taking a feed items and filter them.

object FilterA {
  case class Getfilter(feedInfo: Handler.FeedInfo, since: String)
   extends RequestMsg
  case class  FilterResponse(feed: Handler.FeedInfo) extends ResponseMsg
}

class FilterA extends Actor {

  def receive = {
    case FilterA.Getfilter(feedInfo, since) => {
      
      var backhome = sender()    
      
      // Set the formats of date
      val date: SimpleDateFormat = 
        new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", ju.Locale.ENGLISH)

      val parm: SimpleDateFormat = 
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss", ju.Locale.ENGLISH)

      // Parse the parameter to a SimpleFormatDate
      val sc = parm.parse(since)

      val result: Handler.FeedInfo =
        Handler.FeedInfo(
          feedInfo.title,
          feedInfo.description,    
          feedInfo.items.filter(item => date.parse(item.pubDate) 
          .compareTo(sc) > 0) //Compare Param with PubDate 
        )

      backhome ! Status.Success(FilterResponse(result)) 
    }
  }
}