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
import scala.concurrent.Future
import scala.concurrent.duration._
import feedaggregator.server.FeedAggregatorServer._
import akka.Done
import java.text.ParseException


object Handler {
  case object Handle
  case object OK
  case class Bad(msg:String)
  case class Add(url : String, sb: ActorRef)
}


class Handler extends Actor {

  // import scala.reflect.runtime.universe._
  // case class Thing[T](value: T)
  val afeed = context.system.actorOf(Props[FeedA], "FeedA")
  val aggregator = context.system.actorOf(Props[Aggregator], "Aggregator")
  val subscriptions = context.actorOf(Props[Subscriptions], "Subscriptions")

  implicit val system = context.system
  implicit val executionContext = system.dispatcher
  implicit val timeOut = Timeout(20 seconds)

  var flag = 0
  var result:FeedInfo = FeedInfo("",Some(""),List())
  
  def receive = {
    case Handler.Handle => {
      val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
      println(s"Server ON at http://localhost:8080/\nPress RETURN to stop...")
      StdIn.readLine() // let it run until user presses return
      bindingFuture
        .flatMap(_.unbind()) // trigger unbinding from the port
        .onComplete(_ => system.terminate()) // and shutdown when done
    }
  }
  
  var status: String = ""
  var res:List[FeedInfo] = List()
    
  val route =
    concat (
      path("") {
        complete("Hello, World!\n")
      },
      path("feed") {
        get {
          parameter("url".as[String]) { url => 
            parameter("since" ? "0") {since =>
              try {
              
                var futfeed = (afeed ? FeedA.Parsear(url,since))
                
                futfeed.onComplete {
                    case Success(value) => value match {
                      case value:FeedInfo => {
                        flag = 0
                        result = value
                      }
                      case Handler.Bad(value) => {
                        flag = 1
                        status = value
                      }
                    }
                    case Failure(value) => {
                      flag = 1
                      status = s"400 - Bad Request: can't parse ${url}\n"
                    }
                  }
                  
                  while (futfeed.isCompleted == false) {}
                  if (flag == 0) {
                    flag = 1
                    complete(result)
                  } else {
                    complete(status)
                  }
              } catch {
                case e:Exception => complete(e.getMessage())
              }
            }
          }
        }
      },
      path("subscribe") {
        post {
          parameter("url".as[String]) { url =>
            try {
              (aggregator ? Handler.Add(url, subscriptions))
              .onComplete {
                case Success(value) => value match {
                  case Handler.OK => status = "200 - OK\n"
                  case Handler.Bad(msg) => status = msg
                }
                case Failure(value) => status = "400 - Bad Request\n"
              }
              complete(status)
            }
            catch {
              case e:Exception => complete(e.getMessage())
            }
          }
        }
      },
      path("feeds") {
        get {
          parameter ("since" ? "0") {since =>
            val fut = (subscriptions ? Subscriptions.Feeds(since))
            fut.onComplete {
              case Success(value) => value match{
                case value:List[FeedAggregatorServer.FeedInfo] => {
                  res = value
                  println("ASDSAD"+res)
                }
                case Handler.Bad(msg) => status = msg
              } 
              case Failure(e) => println("...")
            }
            while (fut.isCompleted == false) {}
            complete(res)
          }
        }
      }
    )

}