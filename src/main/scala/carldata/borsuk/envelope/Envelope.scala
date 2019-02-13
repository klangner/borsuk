package carldata.borsuk.envelope

import java.nio.file.Paths
import java.time.{Duration, LocalDate}

import carldata.borsuk.envelope.ApiObjects.FitEnvelopeParams
import carldata.borsuk.envelope.EnvelopeFileContentJsonProtocol.EnvelopeFileContentFormat
import carldata.borsuk.helper._
import carldata.borsuk.rdiis._
import carldata.borsuk.storms.Storms
import carldata.series.{Sessions, TimeSeries}
import org.slf4j.LoggerFactory
import smile.regression.OLS
import spray.json._

import scala.collection.immutable
import scala.collection.immutable.HashMap

class Envelope(modelType: String, id: String) {
  private var stormIntensityWindow: Duration = Duration.ofHours(6)
  private var flowIntensityWindow: Duration = Duration.ofHours(1)
  private var dryDayWindow = Duration.ofHours(48)
  private val Log = LoggerFactory.getLogger("Envelope")

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
    Log.debug("List for model: " + this.id)
    model
  }

  def alignTimeSeries(xs: TimeSeries[Double], ys: TimeSeries[Double]): (TimeSeries[Double], TimeSeries[Double]) = {
    val ts = xs.join(ys)
    (TimeSeries(ts.index, ts.values.map(_._1)), TimeSeries(ts.index, ts.values.map(_._2)))
  }

  def fit(params: FitEnvelopeParams, rdii: RDII): Unit = {
    Log.debug("Start Fit model: " + this.id)
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

      val (rainfall2, flow2) = alignTimeSeries(rainfall, flow)

      rdii.model = immutable.HashMap.empty[String, RDIIObject]

      val envelopes = createStorms(rainfall2, minSessionWindow, maxSessionWindow, stormIntensityWindow)
        .zipWithIndex
        .map {
          sessionWindowAndStorm =>
            val storms: Seq[(String, Storms.StormParams)] = sessionWindowAndStorm._1._2
            val sessionWindow = sessionWindowAndStorm._1._1
            val rdiis = createRdiis(storms, rainfall2, flow2)

            rdii.model ++= immutable.HashMap(rdiis.map(x => (x._1, x._2)): _*)

            val dataPoints: Seq[((Sessions.Session, Double), Double)] = storms.sortBy(_._1).zip(rdiis.sortBy(_._1)).map {
              stormWithRdii =>
                val session: Sessions.Session = stormWithRdii._1._2.session
                val inflow: TimeSeries[Double] = stormWithRdii._2._3
                  .rollingWindow(flowIntensityWindow.minusSeconds(1), x => x.sum / x.length)
                ((session, Storms.maxIntensity(session, rainfall2, stormIntensityWindow)), Inflow.intensity(inflow))
            }.filter(_._2 > params.flowBoundary)
              .sortBy(_._1._2)
              .reverse


            val r = calculateCoefficients(dataPoints.take(15))
            (sessionWindowAndStorm._2.toString, new EnvelopeResult(dataPoints, r, sessionWindow))
        }.toList

      rdii.buildNumber += 1
      rdii.save()
      model = immutable.HashMap(envelopes: _*)
      buildNumber += 1
      save()
    }
    Log.debug("Stop Fit model: " + this.id)
  }

  def save() {
    Log.debug("Save model: " + this.id)
    val path = Paths.get("/borsuk_data/envelopes/", this.modelType)
    val envelopeFileContent = new EnvelopeFileContent(this.model, buildNumber)
    //New Binary format version
    PVCHelper.saveModelBinary[EnvelopeFileContent](path, this.id, envelopeFileContent)
    //Old JSON format version
    //val model = Model(this.modelType, this.id, envelopeFileContent.toJson(EnvelopeFileContentFormat).toString)
    //PVCHelper.saveModel(path, model)
    Log.debug("Model: " + this.id + " saved")
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
                   , maxSessionWindow: Duration
                   , intensityWindow: Duration): Map[Duration, List[(String, Storms.StormParams)]] = {

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
