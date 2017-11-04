package pl.klangner.dss

import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import org.slf4j.LoggerFactory
import spray.json.{JsString, JsValue, JsonParser}


class DataRoute(dataPath: String) {

  private val Log = LoggerFactory.getLogger(getClass.getName)

  val storage = new FileStorage(dataPath)

  def addData(str: String): StandardRoute = {
    val fields = JsonParser(str).asJsObject.fields
    val dataset = stringValue(fields("dataset"))
    val data = fields("data").asJsObject.compactPrint
    storage.add(dataset, data)
    complete("1")
  }

  private def stringValue(json: JsValue): String = json match {
    case JsString(value) => value
    case _ => json.toString()
  }

}