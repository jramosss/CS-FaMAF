package feedaggregator.server

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Success, Failure, Try}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.util.Timeout
import akka.actor.{ActorSystem, Actor, ActorRef, Props,
  ActorContext, Status}
import akka.pattern.ask
import spray.json.DefaultJsonProtocol._
import scala.io.StdIn
import feedaggregator.actors._
import java.text.SimpleDateFormat
import java.{util=>ju}
import java.net._
import akka.http.javadsl.model.StatusCode

object Handler {

  // Set the formats of the feeds
  final case class FeedItem(title: String, link: String, 
    description: Option[String], pubDate: String)
  final case class FeedInfo(title: String, 
    description: Option[String], items: List[FeedItem])

  // Needed for Unmarshalling
  implicit val feedItem = jsonFormat4(FeedItem)
  implicit val feedInfo = jsonFormat3(FeedInfo)

  // Actor context
  val system = FeedAggregatorServer.system
  implicit val timeOut = Timeout(15.seconds)
  implicit val ec = system.dispatcher

  // Actors 
  val subscriptions = system.actorOf(Props[Subscriptions], "subspcriptions")
  val sync = system.actorOf(Props[SyncA], "sync")
  val feedA = system.actorOf(Props[FeedA], "feedA")
  val aggregator = system.actorOf(Props(classOf[Aggregator],subscriptions), "aggregator")
  
  // Check the since parameter
  private def valid_date(date:String) = {

    val checker: SimpleDateFormat = new SimpleDateFormat(
      "yyyy-MM-d'T'HH:mm:ss", ju.Locale.ENGLISH)
      val result = Try(checker.parse(date))
      result match {
        case Success(_) => true
        case Failure(_) => false
      }
  }

  // This function checks that the url has the right format
  private def valid_url(url: String): Boolean = {

    val urli = Try(new URL(url).toURI)
    urli match {
      case Success(_) => true
      case Failure(_) => false
    }
  }

  // This function verify if url its appropiate
  private def valid_string (str : String) : Boolean = {
    val newstr = Try(new String(str))
    newstr match {
      case Success(_) => true
      case Failure(_) => false
    }
  }

  // Method to response at endpoint Feed 
  def EndPointFeed(url: String, since: String) = {

    // Check if since has the correct format
    valid_date(since) match {
      case true => {
        //Parse the url
        val xmlelem = sync ? SyncA.Sync(url)
        onComplete(xmlelem) {
          case Success(response: ResponseMsg) => response match {
            case SyncA.SyncResponse(feed) => {
              
              // Request to the actor for receive the feed.
              val response = feedA ? FeedA.Parse(feed, since)
  
              onComplete(response) {
                case Success(response: ResponseMsg) => response match {
                  case FeedA.FeedResponse(parsedFeed) =>
                    // Everything its ok
                    complete(
                      StatusCodes.OK -> parsedFeed)
                  
                  case _ => complete(
                    StatusCodes.BadRequest -> "400 - Bad Request")
                }   
                case Failure(exception) =>
                  complete(
                    StatusCodes.BadRequest -> "400 - Bad Request")
              }
  
            }
            case _ => complete(
              StatusCodes.BadRequest -> "400 - Bad Request: Can't parse URL")
          }
          // if SyncRequest fail
          case Failure(exc) => 
            complete(
              StatusCodes.NotFound -> "404 - URL not found")
        }
      }
      case false => complete(
        StatusCodes.BadRequest-> "400 - Bad Request: Can't parse since")
    }
  }

  // Method of giving an response to the endpoint Subscribe 
  def EndPointSubscribe(url: String,username : String) = {
    /*
    TODO hacer funcion getFeed(url) que haga Sync y Parse, ya que 
    estamos repitiendo mucho codigo.
    */
    // Check either url and username.
    valid_url(url) && valid_string(username) match{
      case true => {
        //Get the feed
        val fsync = sync ? SyncA.Sync(url)
  
        onComplete(fsync) {
          case Success(response: ResponseMsg) => response match {
            case SyncA.SyncResponse(feed) => {
  
              //Parse the feed
              val ffeed = feedA ? FeedA.Parse(feed, "0000-01-01T00:00:00")
  
              onComplete(ffeed) {
                case Success(response) => response match {
                  case FeedA.FeedResponse(parsedFeed) => {
  
                    // Call the actor to add the feed to the database
                    val fsubscribe = aggregator ? Aggregator.Add(parsedFeed,username)
                    
                    onComplete(fsubscribe) {
                      case Success(response: ResponseMsg) => response match {
                        //If everything's ok.
                        case Subscriptions.FeedAdded => complete(
                          StatusCodes.OK-> "200 - Ok")
                        //If the username doesn't exists
                        case Subscriptions.NotUser => 
                          complete(StatusCodes.NotFound -> 
                            "404 - username doesn't exists")
  
                        case _ => {
                          complete(
                            StatusCodes.BadRequest->"400 - Bad request") 
                        }
                      }
                      case Failure(e) => complete(
                        StatusCodes.BadRequest -> "400 - Bad Request")
                    }
                  }
                  case _ => complete(
                    StatusCodes.BadRequest->"400 - Bad Request")
                }
                case Failure(exc) => {
                  complete(
                    StatusCodes.BadRequest->"400 - Can't parse URL") }
              }
            }
            case _ => {
              complete(
                StatusCodes.BadRequest->"400 - Can't parse URL") }
          }
          case Failure(exception) => complete(
            // When the url exists and the resource 
            // is invalid also throw a 404
            StatusCodes.NotFound -> s"404 - Not Found: url doesn't exists")
        }
      } 
      case false => complete(
        StatusCodes.BadRequest->"404 - URL not found")
    }
  }

  // Method of giving a response to the endpoint Feeds 
  def EndPointFeeds(since: String, username: String) = {
    val subs = subscriptions

    valid_date(since) match{
      case true => {
        // Call the actor to get the list of feeds.
        val statusFeeds = subs ? Subscriptions.Feeds(since, username)
        
        onComplete(statusFeeds) {
  
          case Success(response: ResponseMsg) => response match {
            case Subscriptions.SubsResponse(feeds) => complete(feeds)
            case Subscriptions.NotUser => complete (
              "404 - Not found: username doesn't exist"
            )
            case Subscriptions.NotFeed => complete(
              "404 - Not found: there are no feeds in subscription for " +
                s"${username}")
            case _ => complete(
              "400 - Bad request")
          }
          case Failure(e) => complete(
            e.getMessage()) //TODO See this
        }
      }
      case false => complete("400 - Can't parse since")
    }
  }

  // Method to respond to the endpoint user
  def EndpointUser(username : String) = {
    
    valid_string(username) match {
      case true => {
        // Call the actor to register new username
        val register = (subscriptions ? Subscriptions.Register(username))
        
        onComplete(register) {
          case Success(response: ResponseMsg) => response match {
            // Everythign it ok.
            case Subscriptions.Registered => complete(
              "200 - OK")
            // The username already exists.
            case Subscriptions.AlreadyRegistered => complete(
              "409 - Conflict: username already registered")
          }

          case Failure(e) => complete(
            StatusCodes.BadRequest -> "Internal error")
        }
      }
      case false => complete("Invalid Username")
    }
  }
}