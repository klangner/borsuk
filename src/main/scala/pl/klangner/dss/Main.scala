package pl.klangner.dss

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import org.slf4j.LoggerFactory

import scala.concurrent.Await
import scala.concurrent.duration.Duration


object Main {

  private val Log = LoggerFactory.getLogger(Main.getClass.getName)

  implicit val system: ActorSystem = ActorSystem("dss")
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  case class Params(statsDHost: String)

  /** Parse application arguments */
  def parseArg(args: Array[String]): Params = {
    val statsDHost = stringArg(args, "statsDHost")
    Params(statsDHost)
  }

  /** Parse single argument */
  def stringArg(args: Array[String], key: String, default: String = ""): String = {
    val name = "--" + key + "="
    args.find(_.contains(name)).map(_.substring(name.length)).getOrElse(default).trim
  }

  /** Routing */
  def route(): Route = {

    path("api" / "data") {
       post {
        entity(as[String])(DataRoute.addData)
      }
    } ~ pathPrefix("static") {
      getFromDirectory("static")
    }

  }

  def main(args: Array[String]) {
    val params = parseArg(args)
    StatsD.init("dss", params.statsDHost)

    Log.info("Server started. Open http://localhost:7074/static/index.html")
    Await.result(Http().bindAndHandle(route(), "0.0.0.0", 7074), Duration.Inf)
  }

}