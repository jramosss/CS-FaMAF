package feedaggregator.actors
  
import feedaggregator.server.Handler
import akka.pattern.ask
import akka.util.Timeout
import akka.actor.{ActorContext, ActorSystem, Actor, ActorRef,
  ActorRefFactory, Props, Status}
import scala.util.{Success, Failure}
import scala.concurrent.duration._
import feedaggregator.actors.FeedA.FeedResponse
import feedaggregator.actors.FilterA.FilterResponse

/* This actor have the function of parsing the xml elem and return the feed
 * with the correct format.
 * Then call the filter actor to filter the feeds accordint to the given 
 * since paramater.
 */

object FeedA {
  case class Parse(feed: xml.Elem, since:String) extends RequestMsg
  case class FeedResponse(feed: Handler.FeedInfo) extends ResponseMsg
} 


class FeedA extends Actor {
    
  // Context for filter
  implicit val excontext = context.system.dispatcher
  implicit val timeout = Timeout(20.seconds)

  // Filter actor
  val filterA = context.actorOf(Props[FilterA],"filterA")
  
  def receive = {  
    case FeedA.Parse(feed,since) => {
      //Who callme
      val handler = sender()

      //Fill the info
      val feedInfo = Handler.FeedInfo(
        ((feed \ "channel") \ "title").headOption.map(_.text).get,
        ((feed \ "channel") \ "description").headOption.map(_.text),
        ((feed \ "channel") \\ "item").map(item =>              
          Handler.FeedItem(
            (item \ "title").headOption.map(_.text).get,
            (item \ "link").headOption.map(_.text).get,
            (item \ "description").headOption.map(_.text),
            (item \ "pubDate").headOption.map(_.text).get)
        ).toList
      )

      // Call to filter
      (since == "0000-01-01T00:00:00") match {
        case false => {
          (filterA ? FilterA.Getfilter(feedInfo, since)).onComplete {
            case Success(response: ResponseMsg) => response match {
              case FilterA.FilterResponse(value) =>            
                handler ! Status.Success(FeedResponse(value))
              case _ => // Check what can do 
            } 
            case Failure(exception) => 
              handler ! Status.Failure(exception)  
          }           
        }
        case true => handler ! Status.Success(FeedResponse(feedInfo))       
      }
    }   
  }
}  