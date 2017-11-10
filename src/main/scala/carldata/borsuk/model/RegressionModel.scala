package carldata.borsuk.model

import carldata.borsuk.{DatasetStorage, Jsons}
import spray.json._


class RegressionModel(name: String, storage: DatasetStorage) extends Model[Float] {

  /** Prediction model based on the last value */
  var lastValue: Float = 0
  /** Last score */
  var score: Float = 0f


  override def update(data: String): Unit = {
    val json = data.parseJson
    val features = getFeatures(json)
    val targetValue = getTargetValue(json)
    val prediction = predict(features)
    updateScore(prediction, targetValue)
    updateModel(features, targetValue)
  }

  private def getFeatures(value: JsValue): String = value match {
    case JsObject(fields) => Jsons.stringFromValue(fields("index"))
    case _ => ""
  }

  private def getTargetValue(value: JsValue): Float = value match {
    case JsObject(fields) => Jsons.floatFromValue(fields("value"))
    case _ => 0
  }

  /** Predict value based on given features */
  def predict(data: String): Float = lastValue

  /** Update score metric */
  def updateScore(prediction: Float, target: Float): Unit = {
    score = math.abs(prediction-target)
  }

  /** Update model */
  def updateModel(features: String, target: Float): Unit = {
    lastValue = target
  }
}
