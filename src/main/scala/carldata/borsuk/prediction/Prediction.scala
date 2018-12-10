package carldata.borsuk.prediction

import java.time.LocalDate

import carldata.series.TimeSeries

/** Prediction for the time series data */
class Prediction(modelType: String, id: String) {
  var model: Option[trainedModel] = None
  var buildNumber: Int = 0
  var score: Double = 0.0

  /** Fit model */
  def fit(flow: TimeSeries[Double], rainfall: TimeSeries[Double]): Unit = {
    if (flow.nonEmpty && rainfall.nonEmpty) {
      val features = DailyPatternModel.fit(flow)
      model = Some(features)
      buildNumber += 1
    }
  }

  /** Predict values for the next 24h */
  def predict(day: LocalDate): Array[Double] = {
    model.map {
      DailyPatternModel.predict(day, _)
        .features
        .values.unzip._1.toArray
    }
      .getOrElse(Array())
  }
}
