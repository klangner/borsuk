package carldata.borsuk

import java.util.UUID.randomUUID

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import spray.json._


class ModelAPI() {
  val models = collection.mutable.Map.empty[String, String]

  def create(body: String): StandardRoute = {
    if (body.isEmpty) complete("type:error\nEmpty body.")
    else {
      val json = body.parseJson.asJsObject
      if (json.fields.contains("type")) {
        val id = randomUUID().toString
        models += id -> "props"

        complete("{\"id\": \"" + id + "\"}")
      }
      else complete("type:error\nModel type not provided.")
    }
  }

  def predict(id: String, body: String): StandardRoute = {
    if (models.contains(id)) {
      if (body.isEmpty) complete("type:error\nEmpty body.")
      else {
        val json = body.parseJson.asJsObject
        if (json.fields.contains("features")) {
          complete("{\n  \"labels\": [1,2,3]\n}")
        }
        else complete("type:error\nFeatures not provided.")

      }
    }
    else complete(StatusCodes.NotFound)
  }

  def fit(id: String, body: String): StandardRoute = {
    if (models.contains(id)) {
      if (body.isEmpty) complete("type:error\nEmpty body.")
      else {
        val json = body.parseJson.asJsObject
        if (json.fields.contains("features") && json.fields.contains("labels")) {
          complete(StatusCodes.OK)
        }
        else complete("type:error\nFeatures or labels not provided.")
      }
    }
    else complete(StatusCodes.NotFound)
  }

  def status(id: String): StandardRoute = {
    if (models.contains(id)) {
      complete("{\n  \"build\": \"1\"\n}")
    }
    else complete(StatusCodes.NotFound)
  }

}
