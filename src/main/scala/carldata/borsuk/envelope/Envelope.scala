package carldata.borsuk.envelope

import java.time.{Duration, LocalDate}

import carldata.borsuk.envelope.ApiObjects.FitEnvelopeParams
import carldata.borsuk.helper.{DateTimeHelper, TimeSeriesHelper}
import carldata.borsuk.rdiis._
import carldata.borsuk.storms.Storms
import carldata.series.{Sessions, TimeSeries}
import smile.regression.OLS

import scala.collection.immutable.HashMap

case class EnvelopeObject(sessionWindow: Duration)

class Envelope(modelType: String) {
  var model: Map[String, EnvelopeObject] = HashMap.empty[String, EnvelopeObject]
  var buildNumber: Int = 0

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

      val ts = rainfall.slice(rainfall.index.head, rainfall.index.last)
        .join(flow.slice(rainfall.index.head, rainfall.index.last))
      val rainfall2 = TimeSeries(ts.index, ts.values.map(_._1))


      val envelopes = EnvelopeBuilder(rainfall2, flow, minSessionWindow, maxSessionWindow).build()
      envelopes.foreach(rdii.model ++= _._3)

      //println(envelopes.head._1.dataPoints)
    }

    buildNumber += 1
  }

}


case class EnvelopeBuilder(rainfall: TimeSeries[Double], flow: TimeSeries[Double]
                           , minSessionWindow: Duration, maxSessionWindow: Duration) {

  //private var stormSessionWindows: Duration = Duration.ofHours(12)
  private var stormIntensityWindow: Duration = Duration.ofHours(6)
  private var flowIntensityWindow: Duration = Duration.ofHours(1)
  private var dryDayWindow = Duration.ofHours(48)
  private var flowBoundary = 3.0

  def withFlowBoundary(fb: Double): EnvelopeBuilder = {
    flowBoundary = fb
    this
  }

/*  def withStormSessionWindows(window: Duration): EnvelopeBuilder = {
    stormSessionWindows = window
    this
  }*/

  def withStormIntensityWindow(window: Duration): EnvelopeBuilder = {
    stormIntensityWindow = window
    this
  }

  def withFlowIntensityWindow(window: Duration): EnvelopeBuilder = {
    flowIntensityWindow = window
    this
  }

  def withDryDayWindow(window: Duration): EnvelopeBuilder = {
    dryDayWindow = window
    this
  }

  def calculateCoefficients(xs: Seq[((Sessions.Session, Double), Double)]): Seq[Double] = {
    if (xs.size > 1) {
      val (rainfalls, flows) = xs.toArray.unzip
      val ols = new OLS(rainfalls.map(x => Array(x._2)), flows)
      Seq(ols.coefficients().head, ols.intercept(), ols.adjustedRSquared())
    }
    else Seq(0.0, 0.0, 0.0)
    }

  def alignTimeSeries(xs: TimeSeries[Double], ys: TimeSeries[Double]): (TimeSeries[Double], TimeSeries[Double]) = {
    val ts = xs.join(ys)
    (TimeSeries(ts.index, ts.values.map(_._1)), TimeSeries(ts.index, ts.values.map(_._2)))
  }

  def build(): List[(Duration, EnvelopeResult, List[(String, RDIIObject)])] = {
    val allDWPDays: Seq[LocalDate] = DryWeatherPattern.findAllDryDays(rainfall, dryDayWindow)

    //: Seq[(String, StormParams)]
    Storms.getAllStorms(rainfall)
      .filter(x => x._2.sessionWindow.compareTo(minSessionWindow) >= 0)
      .filter(x => x._2.sessionWindow.compareTo(maxSessionWindow) <= 0)
      .groupBy(_._2.sessionWindow).map {
      sessionWindowAndStorm =>
        val storms: Seq[(String, Storms.StormParams)] = sessionWindowAndStorm._2
        val sessionWindow = sessionWindowAndStorm._1
        println("storms number\t" + storms.length)
        println("durs\t" + Storms.getAllStorms(rainfall).map(_._2.sessionWindow).take(5) + "min\t" + minSessionWindow + "max\t" + maxSessionWindow)
        println("durs2\t" + Storms.getAllStorms(rainfall).map(_._2.sessionWindow).head.compareTo(maxSessionWindow) + "min\t" + Storms.getAllStorms(rainfall).map(_._2.sessionWindow).head.compareTo(minSessionWindow))
        val rdiis = storms.map(storm => {
          val r = (storm._1,
            RDIIBuilder(rainfall
              , flow
              , DateTimeHelper.instantToLDT(storm._2.session.startIndex)
              , DateTimeHelper.instantToLDT(storm._2.session.endIndex)
              , allDWPDays
            )
              .build())
          println(r._1 + "\tis done")
          r
        }
        )

        // Sequence of pair storm intensity & max inflow
        val dataPoints: Seq[((Sessions.Session, Double), Double)] = storms.zip(rdiis).map {
          stormWithRdii =>
            val session = stormWithRdii._1._2.session
            val inflow = stormWithRdii._2._2
              .inflow
              .rollingWindow(flowIntensityWindow.minusSeconds(1), x => x.sum / x.length)
            ((session, Storms.maxIntensity(session, rainfall, stormIntensityWindow)), Inflow.intensity(inflow))
        }.filter(_._2 > flowBoundary)


        val r = calculateCoefficients(dataPoints)
        (sessionWindow, new EnvelopeResult(dataPoints, r), rdiis)
    }

  }
}

class EnvelopeResult(points: Seq[((Sessions.Session, Double), Double)], regression: Seq[Double]) {
  val rainfall: Seq[Double] = this.points.unzip._1.map(_._2)
  val flows: Seq[Double] = this.points.unzip._2
  val dataPoints: Seq[(Double, Double)] = points.map(x => (x._1._2, x._2))
  val slope: Double = this.regression.head
  val intercept: Double = this.regression(1)
  val rSquare: Double = this.regression(2)
  val dates: Seq[Sessions.Session] = this.points.map(_._1._1)
}