package feedaggregator.actors

import akka.actor.{ActorContext, ActorSystem, Actor, ActorRef,
   ActorRefFactory, Props,Status}
import akka.pattern.ask
import scala.util.{Success, Failure}
import scala.concurrent.duration._

import feedaggregator.server.FeedAggregatorServer
import feedaggregator.server.Handler.FeedInfo

// This Actor have the function of calling 
// to the database actor for adding a feed.
 

object Aggregator {
  final case class Add(feed: FeedInfo,username : String) extends RequestMsg
}

/* Takes the ActorRef from param. for making a persitent database 
 * while the server is running.
 */ 
class Aggregator(sb: ActorRef) extends Actor {

  implicit val ec = context.system.dispatcher
  
  var result: FeedInfo = FeedInfo (
    "",Some(""),List()
    )
  
  def receive = {
    case Aggregator.Add(feed,username) => {
      sb forward Subscriptions.Insert(feed,username)
    }
  }
}