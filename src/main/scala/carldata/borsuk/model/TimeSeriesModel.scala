package carldata.borsuk.model

import carldata.borsuk.DatasetStorage
import spray.json._


class TimeSeriesModel(name: String, storage: DatasetStorage) extends Model[Float] {

  /** Prediction model based on the last value */
  var lastValue: Float = 0


  override def update(data: String): Unit = {
    val json = data.parseJson
    val features = getFeatures(json)
    val targetValue = getTargetValue(json)
    val prediction = predict(features)
    updateScore(prediction, targetValue)
    updateModel(features, targetValue)
  }

  /** Predict value based on given features */
  def predict(data: String) = lastValue
}
