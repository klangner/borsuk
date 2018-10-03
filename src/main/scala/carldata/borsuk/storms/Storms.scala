package carldata.borsuk.storms

import java.time.{Duration, Instant, LocalDateTime}
import java.util.UUID.randomUUID

import carldata.borsuk.helper.DateTimeHelper.dtToInstant
import carldata.borsuk.storms.ApiObjects.FitStormsParams
import carldata.series.Sessions.Session
import carldata.series.{Gen, Sessions, TimeSeries}

import scala.collection.mutable

class Storms(modelType: String, id: String) {

  case class StormParams(session: Session, sessionWindow: Duration, values: Vector[Double], childIds: Seq[String])

  var model: mutable.HashMap[String, StormParams] = mutable.HashMap.empty[String, StormParams]
  var buildNumber: Int = 0

  /** Fit model */
  def fit(params: FitStormsParams): Unit = {

    if (params.rainfall.values.nonEmpty) {

      val resolution = params.rainfall.resolution
      val endIndex: LocalDateTime = params.rainfall.startDate.plusSeconds(resolution.getSeconds * params.rainfall.values.length)
      val index: Seq[Instant] = Gen.mkIndex(dtToInstant(params.rainfall.startDate), dtToInstant(endIndex), params.rainfall.resolution)
      val rainfall: TimeSeries[Double] = TimeSeries(index.toVector, params.rainfall.values.toVector)

      val baseSessions: List[(String, StormParams)] = Sessions.findSessions(rainfall)
        .zipWithIndex
        .map(x =>
          x._2.toString ->
            StormParams(x._1, rainfall.resolution, rainfall.slice(x._1.startIndex
                          , x._1.endIndex.plusSeconds(resolution.getSeconds)).values, Seq())
        ).toList

      val listOfSessionWindows: Seq[Duration] =
        baseSessions.map(x => x._2.session.endIndex).zip(baseSessions.tail.map(x => x._2.session.startIndex))
          .map(x => Duration.between(x._1, x._2))
          .distinct.sorted

      def mergeSessions(prev: List[(String, StormParams)], res: List[(String, StormParams)]
                        , sessionWindows: Seq[Duration]): List[(String, StormParams)] = {
        if (sessionWindows.isEmpty) res
        else {
          val sessionWindow = sessionWindows.head
          val next: List[(String, StormParams)] = prev.tail.foldLeft[List[(String, StormParams)]](List(prev.head))((zs, x) => {
            if (sessionWindow.compareTo(Duration.between(zs.head._2.session.endIndex, x._2.session.startIndex)) >= 0) {
              (randomUUID().toString, StormParams(Session(zs.head._2.session.startIndex, x._2.session.endIndex), sessionWindow, zs.head._2.values ++ x._2.values, zs.head._2.childIds ++ Seq(zs.head._1, x._1))
              ) :: zs.tail
            } //merge sessions
            else
              x :: zs
          }).reverse
          mergeSessions(next, res ++ next, sessionWindows.tail)
        }
      }

      model = mutable.HashMap(mergeSessions(baseSessions, baseSessions, listOfSessionWindows): _*)
      buildNumber += 1
    }
  }

  /**
    * List all storms
    */
  def list(sessionWindow: Duration): Seq[(String, Session)] = {
    val lessOrEqualModel = model.filter(x => x._2.sessionWindow.compareTo(sessionWindow) <= 0)
      .toSeq
      .sortBy(_._2.sessionWindow)

    val childIds = lessOrEqualModel.flatMap(_._2.childIds).distinct

    lessOrEqualModel.filter(x => !childIds.contains(x._1))
      .map(x => (x._1, x._2.session))
  }

  /**
    * For provided storm id
    * return maxIntensity
    */
  def get(storm_id: String): Option[(Instant, Instant, Seq[Double])] = {
    model.filter(_._1 == storm_id)
      .map(x => (x._2.session.startIndex, x._2.session.endIndex, x._2.values))
      .headOption
  }
}
