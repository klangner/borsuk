package carldata.borsuk

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

import scala.concurrent.Await
import scala.concurrent.duration.Duration


object Main {

  private val Log = LoggerFactory.getLogger(Main.getClass.getName)

  implicit val system: ActorSystem = ActorSystem("borsuk", ConfigFactory.load())
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  case class Params(dataUrl: String)

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
    val routing = new Routing()

    Await.result(Http().bindAndHandle(routing.route(), "0.0.0.0", 8080), Duration.Inf)
  }

}