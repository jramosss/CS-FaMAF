package feedaggregator.server {

import akka.actor.{ActorSystem, Actor, ActorRef, Props,
  ActorContext}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import scala.io.StdIn
import scala.concurrent.duration._
import akka.util.Timeout

// The Front-end of the server

/* Here the server receives the request for the differents endpoints.
 * The arguments are parsed and send the task to the handler
 */
object FeedAggregatorServer {
  
  implicit val system = ActorSystem()
  implicit val executioncontext = system.dispatcher
  
  def main(args: Array[String]) {

    val route =
      concat (
        path("") {
          complete("Hello, World!")
        },
        path("feed") {
          get {
            parameters('url, 'since.?) { (url, mSince) => 
              {
                val since = mSince.getOrElse("0000-01-01T00:00:00")
                Handler.EndPointFeed(url, since)
              }
            }
          }
        },
        path("subscribe") {
          post {
            parameters('url,'username) { (url,username) => 
              {
                Handler.EndPointSubscribe(url,username)
              }
            }
          }
        },
        path("feeds") {
          get {
            parameters('since.?,'username) { (mSince,username) => 
              {
                val since = mSince.getOrElse("0000-01-01T00:00:00")
                Handler.EndPointFeeds(since,username)
              }
            }
          }
        },
        path("user") {
          post {
            parameters('username) { username =>
              Handler.EndpointUser(username)
            }
          }
        }
      )

    // Server config to listen at the request
    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)
    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
}