package carldata.borsuk.prediction

import java.time.{Duration, LocalDate, LocalDateTime}

import carldata.borsuk.BasicApiObjects.TimeSeriesParams
import carldata.borsuk.helper.TimeSeriesHelper
import carldata.series.TimeSeries
import org.slf4j.LoggerFactory

/** Prediction for the time series data */
class Prediction(modelType: String, id: String) {
  private val emptyTSP = TimeSeriesParams(LocalDateTime.now(), Duration.ZERO, Array())
  private val Log = LoggerFactory.getLogger("Prediction")
  var model: Option[trainedModel] = None
  var buildNumber: Int = 0
  var score: Double = 0.0

  /** Fit model */
  def fit(flow: TimeSeries[Double], rainfall: TimeSeries[Double]): Unit = {
    Log.info("Start Fit model: " + this.id)
    if (flow.nonEmpty && rainfall.nonEmpty) {
      val features = DailyPatternModel.fit(flow)
      model = Some(features)
      buildNumber += 1
    }
    Log.info("Stop Fit model: " + this.id)
  }

  /** Predict values for the next 7 days */
  def predict(startDay: LocalDate): TimeSeriesParams = {
    Log.info("Predict model: " + this.id + " for Start Day: " + startDay)
    model.map { m: trainedModel =>
      val xs = (for (i: Int <- 0 until 7) yield i)
        .map { day =>
          DailyPatternModel.predict(startDay.plusDays(day), m)
            .features
        }
      TimeSeriesHelper.toTimeSeriesParams(TimeSeriesHelper.concat(xs).mapValues(_._1))
    }.getOrElse(emptyTSP)
  }

}
