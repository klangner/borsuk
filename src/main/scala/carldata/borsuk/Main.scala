package carldata.borsuk

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import org.slf4j.LoggerFactory

import scala.collection.immutable.Seq
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


object Main {

  private val Log = LoggerFactory.getLogger(Main.getClass.getName)

  implicit val system: ActorSystem = ActorSystem("borsuk")
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val DATA_TOPIC = "borsuk"
  val POLL_TIMEOUT = 1000

  val settings = CorsSettings.defaultSettings.copy(allowedMethods = Seq(
    HttpMethods.GET,
    HttpMethods.POST,
    HttpMethods.DELETE,
    HttpMethods.HEAD,
    HttpMethods.OPTIONS))

  case class Params(dataUrl: String)

  /** Routing */
  def route(projectsUrl: String): Route = cors(settings) {
    path("api" / "healthcheck") {
      complete("Ok")
    } ~ (path("api" / "prediction" / Remaining) & parameters("flow".as[String], "day".as[String])) {
      (project, flow, day) =>
        get {
          ApiRoutes.predict(project, flow, day, projectsUrl)
        }
    }
  }

  /** Parse application arguments */
  def parseArg(args: Array[String]): Params = {
    val dataUrl = stringArg(args, "data-url", "data")
    Params(dataUrl)
  }

  /** Parse single argument */
  def stringArg(args: Array[String], key: String, default: String = ""): String = {
    val name = "--" + key + "="
    args.find(_.contains(name)).map(_.substring(name.length)).getOrElse(default).trim
  }

  def main(args: Array[String]) {
    val params = parseArg(args)

    // HTTP listener will run in main thread
    Log.info("Server started on port 8080.")
    Await.result(Http().bindAndHandle(route(params.dataUrl), "0.0.0.0", 8080), Duration.Inf)
  }

}