package carldata.borsuk.prediction

import java.util.UUID.randomUUID


class Prediction(modelType: String) {

  val id: String = randomUUID().toString
  var buildNumber: Int = 0
  var score: Double = 0.0

  /** Fit model */
  def fit(data: Array[Double]): Unit = {
    buildNumber += 1
  }

  /** Predict values for the next 24h */
  def predict(): Array[Double] = {
    0.to(24).map(_.toDouble).toArray
  }
}
