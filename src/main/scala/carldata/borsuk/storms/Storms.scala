package carldata.borsuk.storms

import java.time.{Duration, Instant, LocalDateTime}
import java.util.UUID.randomUUID

import carldata.borsuk.helper.DateTimeHelper.dtToInstant
import carldata.borsuk.storms.ApiObjects.FitStormsParams
import carldata.series.Sessions.Session
import carldata.series.{Gen, Sessions, TimeSeries}

import scala.collection.mutable

class Storms(modelType: String, id: String) {

  case class StormParams(id: String, sessionWindow: Duration, values: Seq[String])

  var model: mutable.HashMap[String, (Session, Seq[Double])] = mutable.HashMap.empty[String, (Session, Seq[Double])]
  var stormsList: Seq[StormParams] = Seq()
  var buildNumber: Int = 0

  /** Fit model */
  def fit(params: FitStormsParams): Unit = {

    if (params.rainfall.values.nonEmpty) {

      val resolution = params.rainfall.resolution
      val endIndex: LocalDateTime = params.rainfall.startDate.plusSeconds(resolution.getSeconds * params.rainfall.values.length)
      val index: Seq[Instant] = Gen.mkIndex(dtToInstant(params.rainfall.startDate), dtToInstant(endIndex), params.rainfall.resolution)
      val rainfall: TimeSeries[Double] = TimeSeries(index.toVector, params.rainfall.values.toVector)

      model = mutable.HashMap(Sessions.findSessions(rainfall)
        .zipWithIndex
        .map(x =>
          x._2.toString ->
            (x._1,
              rainfall.slice(x._1.startIndex, x._1.endIndex.plusSeconds(resolution.getSeconds)).values)
        ): _*)

      buildNumber += 1
    }
  }


  /**
    * List all storms
    */
  def list(sessionWindow: Duration): Seq[(String, Session)] = {
    (
      if (!stormsList.exists(_.sessionWindow == sessionWindow)) {
        val modelList: List[(String, Session, Duration, Seq[String])] = model.toList.map(x => (randomUUID().toString, x._2._1, sessionWindow, Seq(x._1)))
        if (model.isEmpty) Seq()
        else
          stormsList ++ modelList.tail.foldLeft[List[(String, Session, Duration, Seq[String])]](List(modelList.head))((zs, x) => {
            if (sessionWindow.compareTo(Duration.between(zs.head._2.endIndex, x._2.startIndex)) >= 0)
              (randomUUID().toString, Session(zs.head._2.startIndex, x._2.endIndex), sessionWindow, zs.head._4 ++ x._4) :: zs.tail //merge sessions
            else
              x :: zs
          }).reverse.map(x => StormParams(x._1, x._3, x._4))
      }
      else {
        stormsList.filter(_.sessionWindow == sessionWindow)
      })
      .map(x => (x.id, Session(model(x.values.head)._1.startIndex, model(x.values.last)._1.endIndex)))
  }

  /**
    * For provided storm id
    * return maxIntensity
    */
  def get(storm_id: String): Option[(Instant, Instant, Seq[Double])] = {
    model.filter(_._1 == storm_id)
      .map(x => (x._2._1.startIndex, x._2._1.endIndex, x._2._2))
      .headOption
  }
}
