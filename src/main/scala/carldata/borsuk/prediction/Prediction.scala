package carldata.borsuk.prediction

import java.util.UUID.randomUUID

import smile.regression.{RandomForest, randomForest}


/** Prediction for the time series data */
class Prediction(modelType: String) {

  val id: String = randomUUID().toString
  var model: Option[RandomForest] = None
  var buildNumber: Int = 0
  var score: Double = 0.0

  /** Fit model */
  def fit(ts: Array[Double]): Unit = {
    if (ts.length > 9) {
      val features: Array[Array[Double]] = ts.indices.map(_ % 24).map(i => Array(i.toDouble)).toArray
      model = Some(randomForest(features, ts, mtry = 1))
      buildNumber += 1
    }
  }

  /** Predict values for the next 24h */
  def predict(): Array[Double] = {
    val features = 0.until(24).map(i => Array(i.toDouble)).toArray
    model.map(_.predict(features)).getOrElse(Array())
  }
}
