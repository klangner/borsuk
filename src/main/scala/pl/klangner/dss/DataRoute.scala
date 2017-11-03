package pl.klangner.dss

import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import org.slf4j.LoggerFactory
import spray.json.{JsArray, JsValue, JsonParser}


object DataRoute {

  private val Log = LoggerFactory.getLogger(getClass.getName)

  val storage = new FileStorage()

  def addData(str: String): StandardRoute = {
    val fields = JsonParser(str).asJsObject.fields
    val dataset = fields("dataset").toString()
    val features = seqFromJsValue(fields("features"))
    val target = fields("target").toString()
    storage.add(dataset, features, target)
    complete("1")
  }

  /** Convert Json array into sequence */
  private def seqFromJsValue(jsVal: JsValue): Seq[String] = jsVal match {
    case JsArray(vs) => vs.map(_.toString)
    case _ => Seq()
  }

}