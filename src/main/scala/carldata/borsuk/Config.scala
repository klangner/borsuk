package carldata.borsuk

import java.io._

import org.slf4j.LoggerFactory
import spray.json._


case class Model(name: String, mType: String)

case class Models(model: List[Model])

object Config extends DefaultJsonProtocol {
  private val Log = LoggerFactory.getLogger(getClass.getName)

  implicit object ConfigJsonFormat extends RootJsonFormat[Models] {

    def write(m: Models) = JsObject()

    def read(value: JsValue): Models = value match {
      case JsObject(fs) =>
        val models = fs.get("models")
          .map(modelsFromValue)
          .getOrElse(List())
        Models(models)
      case _ => Models(List())
    }


    def modelsFromValue(jsVal: JsValue): List[Model] = jsVal match {
      case JsArray(arr) => arr.map {
        case JsObject(obj) =>
          val name: String = obj.get("name").map(stringFromValue).getOrElse("")
          val mType: String = obj.get("type").map(stringFromValue).getOrElse("")
          Model(name, mType)
        case _ => Model("", "")
      }.toList
      case _ => List()
    }

    def stringFromValue(jsVal: JsValue): String = jsVal match {
      case JsString(str) => str
      case v: JsValue => v.toString
    }
  }

  def load(name: String): Option[String] = {
    val cfg = new BufferedReader(new FileReader("config.json"))
      .lines()
      .toArray.mkString
      .parseJson.convertTo[Models]

    cfg.model
      .find(x => x.name == name)
      .map(x => x.mType)

  }


}



