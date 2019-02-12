package carldata.borsuk.storms

import java.nio.file.Paths
import java.time.{Duration, Instant, LocalDateTime}

import carldata.borsuk.helper.DateTimeHelper._
import carldata.borsuk.helper.JsonHelper._
import carldata.borsuk.helper.{Model, PVCHelper, TimeSeriesHelper}
import carldata.borsuk.storms.ApiObjects.FitStormsParams
import carldata.series.Sessions.Session
import carldata.series.{Gen, Sessions, TimeSeries}
import org.slf4j.LoggerFactory
import spray.json._

import scala.annotation.tailrec
import scala.collection.immutable
import scala.collection.immutable.HashMap

object Storms {

  case class StormParams(session: Session, sessionWindow: Seq[Duration], values: Vector[Double])

  private val Log = LoggerFactory.getLogger("Storms")

  /** Get all storms (with merged storms) from rainfall */
  def getAllStorms(rainfall: TimeSeries[Double]
                   , listOfSessionWindows: Option[Seq[Duration]]): List[(String, StormParams)] = {
    if (rainfall.nonEmpty) {
      val baseSessions: List[(Int, StormParams)] = Sessions.findSessions(rainfall)
        .zipWithIndex
        .map(x =>
          x._2 ->
            StormParams(x._1, Seq(rainfall.resolution)
              , TimeSeriesHelper.slice(rainfall, x._1.startIndex
                , x._1.endIndex.plusSeconds(rainfall.resolution.getSeconds)).values)
        ).toList

      if (baseSessions != Nil) {
        val highestIndex = baseSessions.map(_._1).max
        val mergedSession = if (listOfSessionWindows.isDefined) {
          mergeSessions(baseSessions, Set(), listOfSessionWindows.get, rainfall.resolution, highestIndex)
        }
        else {
          val complexListOfSessionWindows: Seq[Duration] =
            baseSessions.map(x => x._2.session.endIndex).zip(baseSessions.tail.map(x => x._2.session.startIndex))
              .map(x => Duration.between(x._1, x._2))
              .distinct.sorted
          mergeSessions(baseSessions, baseSessions.toSet, complexListOfSessionWindows, rainfall.resolution, highestIndex)

        }
        mergedSession
      }
        .map(x => (x._1.toString, x._2))
        .sortBy(_._2.session.startIndex)
      else List()
    }
    else List()
  }

  def joinOrAppend(xs: List[(Int, StormParams)], ys: List[(Int, StormParams)]): Set[(Int, StormParams)] = {
    (xs ++ ys)
      .groupBy(x => x._2.session)
      .map { grouped =>
        (grouped._2.head._1, StormParams(grouped._1, grouped._2.flatMap(x => x._2.sessionWindow), grouped._2.head._2.values))
      }
      .toSet
  }

  def joinOrAppendOld(newSp: (Int, StormParams), xs: List[(Int, StormParams)]): List[(Int, StormParams)] = {
    val index = xs.indexWhere(x => x._2.session == newSp._2.session)
    if (index == -1) { //element with this session not exist yet in the list, lets add it
      newSp :: xs
    }
    else { // element exist so add
      val x = xs(index)
      val sp = StormParams(x._2.session, newSp._2.sessionWindow ++ x._2.sessionWindow, x._2.values)
      xs.updated(index, (xs(index)._1, sp))
    }
  }

  @tailrec
  def mergeSessions(prev: List[(Int, StormParams)], res: Set[(Int, StormParams)]
                    , sessionWindows: Seq[Duration], resolution: Duration
                    , highestIndex: Int): List[(Int, StormParams)] = {
    if (sessionWindows.isEmpty) res.toList
    else {
      val sessionWindow: Duration = sessionWindows.head
      val first: (Int, StormParams) = (prev.head._1
        , StormParams(prev.head._2.session, prev.head._2.sessionWindow, prev.head._2.values))


      val next: List[(Int, StormParams)] = prev.tail.foldLeft[List[(Int, StormParams)]](List(first))((zs, x) => {

        if (sessionWindow.compareTo(Duration.between(zs.head._2.session.endIndex, x._2.session.startIndex)) >= 0) {
          val gapDuration = Duration.between(zs.head._2.session.endIndex, x._2.session.startIndex)

          val gapValues = for (_ <- 1 until (gapDuration.toMillis / resolution.toMillis).toInt) yield 0.0

          val newSp = (highestIndex + x._1
            , StormParams(Session(zs.head._2.session.startIndex, x._2.session.endIndex)
            , Seq(sessionWindow)
            , zs.head._2.values ++ gapValues ++ x._2.values)
          )
          newSp :: zs.tail
          //joinOrAppend(newSp, zs.tail)
        } //merge sessions
        else {
          val newSp: (Int, StormParams) = (highestIndex + x._1, StormParams(x._2.session, Seq(sessionWindow), x._2.values))
          //joinOrAppend(newSp, zs)

          newSp :: zs
        }

      }

      ).reverse
      //res ++ next
      mergeSessions(prev, joinOrAppend(res.toList, next), sessionWindows.tail, resolution, highestIndex + next.length)
    }
  }

  /**
    * Calculate maximum intensity for a single rain event.
    */
  def maxIntensity(session: Session, rainfall: TimeSeries[Double], windowSize: Duration): Double = {
    val xs = TimeSeriesHelper.slice(rainfall, session.startIndex, session.endIndex.plusSeconds(1))
      .rollingWindow(windowSize.minusMillis(1), _.sum)

    if (xs.nonEmpty) xs.dataPoints.maxBy(_._2)._2
    else 0.0
  }
}

/**
  * Storm params json protocol
  */

object StormParamsJsonProtocol extends DefaultJsonProtocol {

  implicit object StormParamsFormat extends RootJsonFormat[Storms.StormParams] {
    def read(json: JsValue): Storms.StormParams = json match {

      case JsObject(x) =>

        val sessionJson: JsObject = x("session").asJsObject

        Storms.StormParams(
          Session(dtToInstant(timestampFromValue(sessionJson.fields("start-date"))),
            dtToInstant(timestampFromValue(sessionJson.fields("end-date")))),
          //Duration.parse(stringFromValue(x("duration"))),
          arrayFromValue(x("duration"), durationFromValue).toSeq,
          arrayFromValue(x("values"), doubleFromValue).toVector
        )
      case _ =>
        Storms.StormParams(Session(dtToInstant(LocalDateTime.now), dtToInstant(LocalDateTime.now)),
          Seq(), Vector())
    }

    def write(obj: Storms.StormParams): JsValue = {
      JsObject(
        "session" -> JsObject(
          "start-date" -> JsString(instantToLDT(obj.session.startIndex).toString),
          "end-date" -> JsString(instantToLDT(obj.session.endIndex).toString)
        ),
        "duration" -> JsArray(obj.sessionWindow.map(x => JsString(x.toString)).toVector),
        //"duration" -> JsString(obj.sessionWindow.toString),
        "values" -> JsArray(obj.values.map(JsNumber(_)))
      )
    }
  }

}

/**
  * Storm params hash map json protocol
  */
object StormParamsHashMapJsonProtocol extends DefaultJsonProtocol {

  import StormParamsJsonProtocol._
  import Storms.StormParams
  import spray.json._

  implicit object StormParamsHashMapFormat extends RootJsonFormat[immutable.HashMap[String, StormParams]] {
    def read(json: JsValue): HashMap[String, StormParams] = {
      val map = json.asInstanceOf[JsArray].elements.map(jsVal =>
        (stringFromValue(jsVal.asJsObject.fields("key")),
          jsVal.asJsObject.fields("value").convertTo[StormParams])).toMap
      val hash = immutable.HashMap.empty
      hash.++(map)
    }

    def write(obj: HashMap[String, StormParams]): JsValue = {
      JsArray(obj.map(x => JsObject(
        "key" -> JsString(x._1),
        "value" -> x._2.toJson
      )).toVector)
    }
  }

}

class Storms(modelType: String, id: String) {

  import carldata.borsuk.storms.StormParamsHashMapJsonProtocol._

  import scala.math.Ordering.Implicits._

  private val Log = LoggerFactory.getLogger("Storms")

  var model: immutable.HashMap[String, Storms.StormParams] = immutable.HashMap.empty[String, Storms.StormParams]
  var buildNumber: Int = 0

  /** Fit model */
  def fit(params: FitStormsParams): Unit = {

    Log.debug("Start Fit model: " + this.id)
    if (params.rainfall.values.nonEmpty) {

      val resolution = params.rainfall.resolution
      val endIndex: LocalDateTime = params.rainfall.startDate.plusSeconds(resolution.getSeconds * params.rainfall.values.length)
      val index: Seq[Instant] = Gen.mkIndex(dtToInstant(params.rainfall.startDate), dtToInstant(endIndex), params.rainfall.resolution)
      val rainfall: TimeSeries[Double] = TimeSeries(index.toVector, params.rainfall.values.toVector)

      val mergedSession: List[(String, Storms.StormParams)] = Storms.getAllStorms(rainfall, None)
      model = immutable.HashMap(mergedSession: _*)
      save()
      buildNumber += 1
      Log.debug("Stop Fit model: " + this.id)
    }
  }

  def save() {
    Log.debug("Save model: " + this.id)
    val path = Paths.get("/borsuk_data/storms/", this.modelType)
    val model = Model(this.modelType, this.id, this.model.toJson(StormParamsHashMapFormat).toString)
    PVCHelper.saveModel(path, model)
    Log.debug("Model: " + this.id + " saved")
  }

  /**
    * List all storms
    */

  def list(sessionWindow: Duration): Seq[(String, Session)] = {
    Log.debug("List for model: " + this.id)

    if (model.nonEmpty) {
      val filterExact = model.filter(_._2.sessionWindow.contains(sessionWindow))
        .map(x => (x._1, x._2.session))
        .toSeq

      if (filterExact.nonEmpty) filterExact
      else {
        //This means that duration is above maximum already listed session windows. Return biggest.
        model.filter(x => x._2.sessionWindow.max.compareTo(sessionWindow) <= 0)
          .groupBy(_._2.sessionWindow)
          .toSeq.maxBy(_._1)._2
          .map(x => (x._1, x._2.session)).toSeq
      }

    }
    else Seq()
  }

  /** Get the storm */
  def get(storm_id: String): Option[(Instant, Instant, Seq[Double])] = {
    Log.debug("Get model: " + this.id + " with Storm_ID: " + storm_id)
    model.filter(_._1 == storm_id)
      .map(x => (x._2.session.startIndex, x._2.session.endIndex, x._2.values))
      .headOption
  }
}
