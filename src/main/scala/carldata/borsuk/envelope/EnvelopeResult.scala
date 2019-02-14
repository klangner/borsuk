package carldata.borsuk.envelope

import java.time.{Duration, Instant}

import carldata.borsuk.helper.DateTimeHelper.{dtToInstant, instantToLDT}
import carldata.borsuk.helper.JsonHelper._
import carldata.series.Sessions
import spray.json.{DefaultJsonProtocol, JsArray, JsNumber, JsObject, JsString, JsValue, RootJsonFormat}

class EnvelopeResult(points: Seq[((Sessions.Session, Double), Double)], regression: Seq[Double], window: Seq[Duration]) extends Serializable {
  val sessionWindow: Seq[Duration] = this.window
  val rainfall: Seq[Double] = this.points.unzip._1.map(_._2)
  val flows: Seq[Double] = this.points.unzip._2
  val dataPoints: Seq[(Double, Double)] = points.map(x => (x._1._2, x._2))
  val slope: Double = this.regression.head
  val intercept: Double = this.regression(1)
  val rSquare: Double = this.regression(2)
  val dates: Seq[Sessions.Session] = this.points.map(_._1._1)
}


object EnvelopeResultJsonProtocol extends DefaultJsonProtocol {

  implicit object EnvelopeResultFormat extends RootJsonFormat[EnvelopeResult] {
    def read(json: JsValue): EnvelopeResult = {
      json match {
        case JsObject(fields) =>

          //val window = Duration.parse(stringFromValue(fields("sessionWindow")))
          val window = arrayFromValue(fields("sessionWindow"), durationFromValue).toSeq
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

        case _ => new EnvelopeResult(Seq(), Seq(), Seq())
      }
    }

    def write(obj: EnvelopeResult): JsValue = {
      JsObject(
        "sessionWindow" -> JsArray(obj.sessionWindow.map(x=> JsString(x.toString)).toVector),
        //"sessionWindow" -> JsString(obj.sessionWindow.toString),
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
