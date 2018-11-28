package carldata.borsuk.envelope

import java.nio.file.Paths
import java.time.{Duration, Instant, LocalDate}
import java.util.UUID.randomUUID

import carldata.borsuk.envelope.ApiObjects.FitEnvelopeParams
import carldata.borsuk.envelope.EnvelopeResultHashMapJsonProtocol.EnvelopeResultHashMapFormat
import carldata.borsuk.helper.DateTimeHelper.{dtToInstant, instantToLDT}
import carldata.borsuk.helper.JsonHelper.{doubleFromValue, stringFromValue, timestampFromValue}
import carldata.borsuk.helper.{DateTimeHelper, Model, PVCHelper, TimeSeriesHelper}
import carldata.borsuk.rdiis._
import carldata.borsuk.storms.Storms
import carldata.series.{Sessions, TimeSeries}
import smile.regression.OLS
import spray.json._

import scala.collection.immutable
import scala.collection.immutable.HashMap

class EnvelopeResult(points: Seq[((Sessions.Session, Double), Double)], regression: Seq[Double], window: Duration) {
  val sessionWindow: Duration = this.window
  val rainfall: Seq[Double] = this.points.unzip._1.map(_._2)
  val flows: Seq[Double] = this.points.unzip._2
  val dataPoints: Seq[(Double, Double)] = points.map(x => (x._1._2, x._2))
  val slope: Double = this.regression.head
  val intercept: Double = this.regression(1)
  val rSquare: Double = this.regression(2)
  val dates: Seq[Sessions.Session] = this.points.map(_._1._1)
}

class Envelope(modelType: String, id: String) {
  private var stormIntensityWindow: Duration = Duration.ofHours(6)
  private var flowIntensityWindow: Duration = Duration.ofHours(1)
  private var dryDayWindow = Duration.ofHours(48)

  var model: immutable.HashMap[String, EnvelopeResult] = HashMap.empty[String, EnvelopeResult]
  var buildNumber: Int = 0

  def withStormIntensityWindow(window: Duration): Envelope = {
    stormIntensityWindow = window
    this
  }

  def withFlowIntensityWindow(window: Duration): Envelope = {
    flowIntensityWindow = window
    this
  }

  def withDryDayWindow(window: Duration): Envelope = {
    dryDayWindow = window
    this
  }

  def list(): HashMap[String, EnvelopeResult] = {
    model
  }

  def fit(params: FitEnvelopeParams, rdii: RDII): Unit = {
    if (params.flow.values.nonEmpty && params.rainfall.values.nonEmpty) {

      val flow = TimeSeriesHelper.parse(params.flow).filter(_._2 >= 0)
      val rainfall = TimeSeriesHelper.parse(params.rainfall).filter(_._2 >= 0)

      val minSessionWindow: Duration = if (params.minSessionWindow == Duration.ZERO) {
        Duration.ofHours(12)
      } else {
        params.minSessionWindow
      }
      val maxSessionWindow: Duration = if (params.maxSessionWindow == Duration.ZERO) {
        Duration.ofHours(24)
      } else {
        params.maxSessionWindow
      }

      val rainfall2 = TimeSeriesHelper.adjust(rainfall, flow)

      val envelopes = createStorms(rainfall2, minSessionWindow, maxSessionWindow, stormIntensityWindow).map {
        sessionWindowAndStorm =>
          val storms: Seq[(String, Storms.StormParams)] = sessionWindowAndStorm._2.take(19)
          val sessionWindow = sessionWindowAndStorm._1
          val rdiis = createRdiis(storms, rainfall2, flow)

          rdii.model = immutable.HashMap(rdiis.map(x => (x._1, x._2)): _*)

          val dataPoints: Seq[((Sessions.Session, Double), Double)] = storms.sortBy(_._1).zip(rdiis.sortBy(_._1)).map {
            stormWithRdii =>
              val session: Sessions.Session = stormWithRdii._1._2.session
              val inflow: TimeSeries[Double] = stormWithRdii._2._3
                .rollingWindow(flowIntensityWindow.minusSeconds(1), x => x.sum / x.length)
              ((session, Storms.maxIntensity(session, rainfall, stormIntensityWindow)), Inflow.intensity(inflow))
          }.filter(_._2 > params.flowBoundary)
            .sortBy(_._1._2)
            .reverse


          val r = calculateCoefficients(dataPoints)
          (randomUUID().toString, new EnvelopeResult(dataPoints, r, sessionWindow))
      }.toList

      rdii.save()
      model = immutable.HashMap(envelopes: _*)
      save()
      buildNumber += 1
    }
  }

  def save() {
    val path = Paths.get("/borsuk_data/envelopes/", this.modelType)
    val model = Model(this.modelType, this.id, this.model.toJson(EnvelopeResultHashMapFormat).toString)
    PVCHelper.saveModel(path, model)
  }

  def calculateCoefficients(xs: Seq[((Sessions.Session, Double), Double)]): Seq[Double] = {
    if (xs.size > 1) {
      val (rainfalls, flows) = xs.toArray.unzip
      val ols = new OLS(rainfalls.map(x => Array(x._2)), flows)
      Seq(ols.coefficients().head, ols.intercept(), ols.adjustedRSquared())
    }
    else Seq(0.0, 0.0, 0.0)
  }

  def createStorms(rainfall: TimeSeries[Double], minSessionWindow: Duration
                   , maxSessionWindow: Duration, intensityWindow: Duration): Map[Duration, List[(String, Storms.StormParams)]] = {

    val listOfSessionWindows: Seq[Duration] = for {
      d <- minSessionWindow.toMinutes to maxSessionWindow.toMinutes by Duration.ofMinutes(5).toMinutes
    } yield Duration.ofMinutes(d)

    Storms.getAllStorms(rainfall, Some(listOfSessionWindows))
      .filter(x => x._2.sessionWindow.compareTo(minSessionWindow) >= 0)
      .filter(x => x._2.sessionWindow.compareTo(maxSessionWindow) <= 0)
      .groupBy(_._2.sessionWindow)
      .map {
        s =>
          val sp = s._2.map(x => (x, Storms.maxIntensity(x._2.session, rainfall, intensityWindow)))
            .filter(_._2 > 0)
            .sortBy(-_._2).toVector
            .map(_._1).toList
          s._1 -> sp
      }

  }

  def createRdiis(storms: Seq[(String, Storms.StormParams)], rainfall: TimeSeries[Double]
                  , flow: TimeSeries[Double]): Seq[(String, RDIIObject, TimeSeries[Double])] = {
    val allDWPDays: Seq[LocalDate] = DryWeatherPattern.findAllDryDays(rainfall, dryDayWindow)
    storms.map(storm => {
      val builder = RDIIBuilder(rainfall
        , flow
        , DateTimeHelper.instantToLDT(storm._2.session.startIndex)
        , DateTimeHelper.instantToLDT(storm._2.session.endIndex).minusDays(1)
        , allDWPDays
      )

      (storm._1,
        builder.build(), builder.inflow)
    })
  }

}

object EnvelopeResultJsonProtocol extends DefaultJsonProtocol {

  implicit object EnvelopeResultFormat extends RootJsonFormat[EnvelopeResult] {
    def read(json: JsValue): EnvelopeResult = {
      json match {
        case JsObject(fields) =>

          val window = Duration.parse(stringFromValue(fields("sessionWindow")))

          val rainfall: Seq[Double] = fields("rainfall") match {
            case JsArray(elements) => elements.map(doubleFromValue)
            case _ => Seq()
          }

          val flows: Seq[Double] = fields("flows") match {
            case JsArray(elements) => elements.map(doubleFromValue)
            case _ => Seq()
          }

          val slope: Double = doubleFromValue(fields("slope"))
          val intercept: Double = doubleFromValue(fields("intercept"))
          val rSquare: Double = doubleFromValue(fields("rSquare"))

          val dates: Seq[Sessions.Session] = fields("dates") match {
            case JsArray(elements) => elements.map {
              case JsObject(timeFrame) => Sessions.Session(
                dtToInstant(timestampFromValue(timeFrame("start-date"))),
                dtToInstant(timestampFromValue(timeFrame("end-date")))
              )
              case _ => Sessions.Session(Instant.MIN, Instant.MIN)
            }
            case _ => Seq()
          }

          val points: Seq[((Sessions.Session, Double), Double)] = dates.zip(rainfall).zip(flows)
          val regression = Seq(slope, intercept, rSquare)

          new EnvelopeResult(points, regression, window)

        case _ => new EnvelopeResult(Seq(), Seq(), Duration.ZERO)
      }
    }

    def write(obj: EnvelopeResult): JsValue = {
      JsObject(
        "sessionWindow" -> JsString(obj.sessionWindow.toString),
        "rainfall" -> JsArray(obj.rainfall.map(JsNumber(_)).toVector),
        "flows" -> JsArray(obj.flows.map(JsNumber(_)).toVector),
        "dataPoints" -> JsArray(obj.dataPoints.map(x => JsObject(
          "val1" -> JsNumber(x._1),
          "val2" -> JsNumber(x._2)
        )).toVector),
        "slope" -> JsNumber(obj.slope),
        "intercept" -> JsNumber(obj.intercept),
        "rSquare" -> JsNumber(obj.rSquare),
        "dates" -> JsArray(obj.dates.map(x => JsObject(
          "start-date" -> JsString(instantToLDT(x.startIndex).toString),
          "end-date" -> JsString(instantToLDT(x.endIndex).toString)
        )).toVector)
      )
    }
  }

}

object EnvelopeResultHashMapJsonProtocol extends DefaultJsonProtocol {

  import EnvelopeResultJsonProtocol._

  implicit object EnvelopeResultHashMapFormat extends RootJsonFormat[immutable.HashMap[String, EnvelopeResult]] {

    def read(json: JsValue): HashMap[String, EnvelopeResult] = {
      json match {
        case JsArray(elements) =>
          val pairs = elements.map {
            case JsObject(fields) => (
              stringFromValue(fields("key")),
              fields("value").convertTo[EnvelopeResult])
            case _ => ("", new EnvelopeResult(Seq(), Seq(), Duration.ZERO))
          }.toMap

          val hash = immutable.HashMap.empty
          hash.++(pairs)
        case _ => immutable.HashMap.empty[String, EnvelopeResult]
      }
    }

    def write(obj: HashMap[String, EnvelopeResult]): JsValue = {
      JsArray(obj.map(x => JsObject(
        "key" -> JsString(x._1),
        "value" -> x._2.toJson
      )).toVector)
    }
  }

}
