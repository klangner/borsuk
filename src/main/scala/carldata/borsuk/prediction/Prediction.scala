package carldata.borsuk.prediction

import java.util.UUID.randomUUID


class Prediction(modelType: String) {

  val id: String = randomUUID().toString

  /** Fit model */
  def fit(data: Vector[Float]): Unit = {

  }
}
