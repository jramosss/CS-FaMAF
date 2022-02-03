package feedaggregator.actors

import scala.collection.mutable.Map
import akka.actor.{ActorContext, ActorSystem, Actor, ActorRef, Props,Status}
import akka.pattern.ask 
import akka.util.Timeout
import scala.language.postfixOps 
import scala.util.{Success, Failure}
import scala.concurrent.duration._
import feedaggregator.server.{FeedAggregatorServer,Handler}
import feedaggregator.actors.Subscriptions.NotFeed
import feedaggregator.server.Handler.FeedInfo
import feedaggregator.actors.Subscriptions.NotUser
import feedaggregator.actors.Subscriptions.FeedAdded

/* This actor have the function of be the database, persistant while the 
 * server is on.
 * Make the tasks of interact with data base, answering the request
 * either to add or deliver feeds
 */

object Subscriptions {
  final case class Feeds(since: String, username: String) extends RequestMsg
  final case class Insert(feed: Handler.FeedInfo, username: String) 
    extends RequestMsg
  final case class Register(username: String) extends RequestMsg
  final case class SubsResponse(feeds: List[Handler.FeedInfo])
    extends ResponseMsg
  final case object AlreadyRegistered extends ResponseMsg
  final case object Registered extends ResponseMsg
  final case object NotFeed extends ResponseMsg
  final case object NotUser extends ResponseMsg
  final case object FeedAdded extends ResponseMsg
}

class Subscriptions extends Actor {
  
  //Create a database for the first time the actor is called
  var db: Map[String, List[FeedInfo]] = Map()
  implicit val ec = context.system.dispatcher
  implicit val timeOut = Timeout(20 seconds)
  val filterA = context.system.actorOf(Props[FilterA],"FilterA")

  def receive = {
    /* If user doesn't exist then add to the db with a empty list of feeds
     * If user already exists then send back to the sender
     */
    case Subscriptions.Register(username) => {
      db.contains(username) match {
        case true => sender ! Status.Success(Subscriptions.AlreadyRegistered)
        case false => {
          db.addOne((username, List()))
          sender ! Status.Success(Subscriptions.Registered)
        }
      }
    } 

    /* If user exists, insert in the key (username) the list of 
     * previous feeds concatenated with the new one
     * with the "put" method which replaces the existing value
     * (List[FeedInfo])
     */  
    case Subscriptions.Insert(feed, username) => {
      db.contains(username) match{
        case true => {
          db.get(username).get.contains(feed) match {
            case true => sender ! Status.Success(FeedAdded)
            case false => {
              db.put(username, db.get(username).get.:::(List(feed)))
              sender ! Status.Success(FeedAdded)
            }
          }
        }
        case false => sender ! Status.Success(NotUser)
      }
    }

    /* If the username exists and its list of feed aren't empty then return
     * the list of feeds
     * If not then respond to the handler
     */
case Subscriptions.Feeds(since, username) => {
      var result: List[Handler.FeedInfo] = List()
      val handler = sender()
      var feeds_processed = 0

      db.contains(username) match {
        case false => handler ! Status.Success(NotUser)
        case true => {
          db.get(username).get.isEmpty match {
            case true => handler ! Status.Success(NotFeed)
            case false => {

              (since == "0000-01-01T00:00:00") match {
                case false => {
                  val list = db.get(username).get
                  list.foreach(feed =>
                    //Filter according to the since parameter
                    (filterA ? FilterA.Getfilter(feed, since)).onComplete {

                      case Success(response: ResponseMsg) => response match {
                        case FilterA.FilterResponse(filteredFeed) => {
                         /* If all went right and my value is FeedInfo Type,
                          * then i add it straight to the result
                          */
                          result = result ::: List(filteredFeed)
                          feeds_processed += 1
                          /* When I have processed all the elements, 
                          * i send the result
                          */
                          if (feeds_processed == list.length) {
                            handler ! Subscriptions.SubsResponse(result)
                          }
                        }
                      }
                      case Failure(msg) => handler ! Status.Failure(msg)
                    } 
                  )
                }
                case true => 
                  handler ! Subscriptions.SubsResponse(db.get(username).get)
              }
            }
          }
        }
      }
    }
  }
}