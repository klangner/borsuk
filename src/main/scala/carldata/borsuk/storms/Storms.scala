package carldata.borsuk.storms

import java.time.{Instant, LocalDateTime}

import carldata.borsuk.helper.DateTimeHelper.dtToInstant
import carldata.borsuk.storms.ApiObjects.FitStormsParams
import carldata.series.Sessions.Session
import carldata.series.{Gen, TimeSeries}

class Storms(modelType: String, id: String) {
  var model: Seq[(String, Session, Seq[Double])] = Seq()
  var buildNumber: Int = 0

  /** Fit model */
  def fit(params: FitStormsParams): Unit = {

    if (params.rainfall.nonEmpty) {

      val endIndex: LocalDateTime = params.startDate.plusSeconds(params.resolution.getSeconds * params.rainfall.length)
      val index: Seq[Instant] = Gen.mkIndex(dtToInstant(params.startDate), dtToInstant(endIndex), params.resolution)
      val rainfall: TimeSeries[Double] = TimeSeries(index.toVector, params.rainfall.toVector)
      model = Seq() //TODO: fit model ( tip, use Sessions.findSessions(ts: TimeSeries[V])

      buildNumber += 1
    }
  }


  /**
    * List all storms
    */
  def list(): Seq[(String, Session)] = {
    model.map(x => (x._1, x._2))
  }

  /**
    * For provided storm id
    * return maxIntensity
    */
  def get(storm_id: String): Option[(Instant, Instant, Seq[Double])] = {
    model.filter(_._1 == storm_id)
      .map(x => (x._2.startIndex, x._2.endIndex, x._3))
      .headOption
  }
}
