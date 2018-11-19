package carldata.borsuk.envelope

import java.time.{Duration, LocalDate}
import java.util.UUID.randomUUID

import carldata.borsuk.envelope.ApiObjects.FitEnvelopeParams
import carldata.borsuk.helper.{DateTimeHelper, TimeSeriesHelper}
import carldata.borsuk.rdiis._
import carldata.borsuk.storms.Storms
import carldata.series.{Sessions, TimeSeries}
import smile.regression.OLS

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

class Envelope(modelType: String) {
  private var stormIntensityWindow: Duration = Duration.ofHours(6)
  private var flowIntensityWindow: Duration = Duration.ofHours(1)
  private var dryDayWindow = Duration.ofHours(48)
  private var flowBoundary = 3.0

  var model: immutable.HashMap[String, EnvelopeResult] = HashMap.empty[String, EnvelopeResult]
  var buildNumber: Int = 0

  def withFlowBoundary(fb: Double): Envelope = {
    flowBoundary = fb
    this
  }

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

      val flow = TimeSeriesHelper.parse(params.flow)
      val rainfall = TimeSeriesHelper.parse(params.rainfall)

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

      val envelopes = createStorms(rainfall2, minSessionWindow, maxSessionWindow).map {
        sessionWindowAndStorm =>
          val storms: Seq[(String, Storms.StormParams)] = sessionWindowAndStorm._2
          val sessionWindow = sessionWindowAndStorm._1

          val rdiis = createRdiis(storms, rainfall2, flow)

          rdii.model ++= immutable.HashMap(rdiis: _*)

          val dataPoints: Seq[((Sessions.Session, Double), Double)] = storms.zip(rdiis).map {
            stormWithRdii =>
              val session: Sessions.Session = stormWithRdii._1._2.session
              val inflow: TimeSeries[Double] = stormWithRdii._2._2
                .inflow
                .rollingWindow(flowIntensityWindow.minusSeconds(1), x => x.sum / x.length)
              ((session, Storms.maxIntensity(session, rainfall, stormIntensityWindow)), Inflow.intensity(inflow))
          }.filter(_._2 > flowBoundary)
            .sortBy(_._1._2)
            .reverse

          val r = calculateCoefficients(dataPoints)
          (randomUUID().toString, new EnvelopeResult(dataPoints, r, sessionWindow))
      }.toList

      model = immutable.HashMap(envelopes: _*)
      buildNumber += 1
    }


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
                   , maxSessionWindow: Duration): Map[Duration, List[(String, Storms.StormParams)]] = {

    val listOfSessionWindows: Seq[Duration] = for {
      d <- minSessionWindow.toMinutes to maxSessionWindow.toMinutes by Duration.ofMinutes(5).toMinutes
    } yield Duration.ofMinutes(d)

    Storms.getAllStorms(rainfall, Some(listOfSessionWindows))
      .filter(x => x._2.sessionWindow.compareTo(minSessionWindow) >= 0)
      .filter(x => x._2.sessionWindow.compareTo(maxSessionWindow) <= 0)
      .groupBy(_._2.sessionWindow)

  }

  def createRdiis(storms: Seq[(String, Storms.StormParams)], rainfall: TimeSeries[Double]
                  , flow: TimeSeries[Double]): Seq[(String, RDIIObject)] = {
    val allDWPDays: Seq[LocalDate] = DryWeatherPattern.findAllDryDays(rainfall, dryDayWindow)
    storms.map(storm => {
      (storm._1,
        RDIIBuilder(rainfall
          , flow
          , DateTimeHelper.instantToLDT(storm._2.session.startIndex)
          , DateTimeHelper.instantToLDT(storm._2.session.endIndex)
          , allDWPDays
        ).build())
    })
  }

}
