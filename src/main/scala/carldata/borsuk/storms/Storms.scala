package carldata.borsuk.storms

import java.time.{Duration, Instant, LocalDateTime}
import java.util.UUID.randomUUID

import carldata.borsuk.helper.DateTimeHelper.dtToInstant
import carldata.borsuk.storms.ApiObjects.FitStormsParams
import carldata.series.Sessions.Session
import carldata.series.{Gen, Sessions, TimeSeries}

class Storms(modelType: String, id: String) {
  var model: Seq[(String, Session, Seq[Double])] = Seq()
  var stormsList: Seq[(String, Duration, Seq[String])] = Seq()
  var buildNumber: Int = 0

  /** Fit model */
  def fit(params: FitStormsParams): Unit = {

    if (params.rainfall.values.nonEmpty) {

      val resolution = params.rainfall.resolution
      val endIndex: LocalDateTime = params.rainfall.startDate.plusSeconds(resolution.getSeconds * params.rainfall.values.length)
      val index: Seq[Instant] = Gen.mkIndex(dtToInstant(params.rainfall.startDate), dtToInstant(endIndex), params.rainfall.resolution)
      val rainfall: TimeSeries[Double] = TimeSeries(index.toVector, params.rainfall.values.toVector)

      model = Sessions.findSessions(rainfall)
        .zipWithIndex
        .map { x =>
          (x._2.toString,
            x._1,
            rainfall.slice(x._1.startIndex, x._1.endIndex.plusSeconds(resolution.getSeconds)).values)
        }

      buildNumber += 1
    }
  }


  /**
    * List all storms
    */
  def list(sessionWindow: Duration): Seq[(String, Session)] = {
    if (!stormsList.exists(_._2 == sessionWindow)) {
      val modelList: List[(String, Session, Duration, Seq[String])] = model.toList.map(x => (randomUUID().toString, x._2, sessionWindow, Seq(x._1)))
      stormsList = if (model.isEmpty) Seq()
      else
        stormsList ++ modelList.tail.foldLeft[List[(String, Session, Duration, Seq[String])]](List(modelList.head))((zs, x) => {
          if (sessionWindow.compareTo(Duration.between(zs.head._2.endIndex, x._2.startIndex)) >= 0)
            (randomUUID().toString, Session(zs.head._2.startIndex, x._2.endIndex), sessionWindow, zs.head._4 ++ x._4) :: zs.tail //merge sessions
          else
            x :: zs
        }).reverse.map(x => (x._1, x._3, x._4))
    }

    stormsList.filter(_._2 == sessionWindow).map(x => (x._1, Session(model.filter(_._1 == x._3.head).map(m => m._2.startIndex).head, model.filter(_._1 == x._3.last).map(m => m._2.endIndex).head)))
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
