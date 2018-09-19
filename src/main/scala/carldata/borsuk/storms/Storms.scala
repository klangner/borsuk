package carldata.borsuk.storms

import java.time.{Duration, Instant, LocalDateTime}
import java.util.UUID.randomUUID

import carldata.borsuk.helper.DateTimeHelper.dtToInstant
import carldata.borsuk.storms.ApiObjects.FitStormsParams
import carldata.series.Sessions.Session
import carldata.series.{Gen, Sessions, TimeSeries}

class Storms(modelType: String) {
  val id: String = randomUUID().toString
  var model: Seq[(String, Session, Double)] = Seq()
  var buildNumber: Int = 0

  /** Fit model */
  def fit(params: FitStormsParams): Unit = {

    if (params.rainfall.nonEmpty) {

      val endIndex: LocalDateTime = params.startDate.plusSeconds(params.resolution.getSeconds * params.rainfall.length)
      val index: Seq[Instant] = Gen.mkIndex(dtToInstant(params.startDate), dtToInstant(endIndex), params.resolution)
      val rainfall: TimeSeries[Double] = TimeSeries(index.toVector, params.rainfall.toVector)
      model = find(rainfall, params.windowSize, params.intensitySize)

      buildNumber += 1
    }
  }

  /**
    * Find all storms sessions with intensity
    */
  def find(rainfall: TimeSeries[Double], windowSize: Duration, intensitySize: Duration): Seq[(String, Session, Double)] = {
    if (rainfall.isEmpty) Seq()
    else {
      val rainSessions = Sessions.findSessions(rainfall, windowSize)

      rainSessions
        .map(x => (randomUUID().toString, x, maxIntensity(x, rainfall, intensitySize)))
        .sortBy(_._3)
        .filter(x => x._3 > 0)
    }
  }

  /**
    * Calculate maximum intensity for a single rain event.
    */
  def maxIntensity(session: Session, rainfall: TimeSeries[Double], windowSize: Duration): Double = {
    val xs = rainfall.slice(session.startIndex, session.endIndex.plusSeconds(1))
      .rollingWindow(windowSize.minusMillis(1), _.sum)

    if (xs.nonEmpty) xs.dataPoints.maxBy(_._2)._2
    else 0.0
  }

  /**
    * List all storms
    */
  def list(): Seq[(String, Session)] = {
    model.map(x => (x._1, x._2)
    )
  }

  /**
    * For provided storm id
    * return maxIntensity
    */
  def get(storm_id: String): Option[(Instant, Instant, Double)] = {
    model.filter(_._1 == storm_id)
      .map(x => (x._2.startIndex, x._2.endIndex, x._3))
      .headOption
  }
}
