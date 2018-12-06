package carldata.borsuk.envelope

import java.time.{Duration, Instant, LocalDateTime}
import java.util.UUID.randomUUID

import carldata.borsuk.BasicApiObjects.TimeSeriesParams
import carldata.borsuk.BasicApiObjectsJsonProtocol._
import carldata.borsuk.helper.DateTimeHelper._
import carldata.borsuk.helper.JsonHelper._
import carldata.series.Sessions
import spray.json._

object ApiObjects {

  case class CreateEnvelopeParams(modelType: String, id: String)

  case class ModelCreatedResponse(id: String)

  case class FitEnvelopeParams(flow: TimeSeriesParams, rainfall: TimeSeriesParams, dryDayWindow: Duration
                               , stormIntensityWindow: Duration, flowIntensityWindow: Duration
                               , minSessionWindow: Duration, maxSessionWindow: Duration
                               , flowBoundary: Double)

  case class EnvelopeObject(id: String, sessionWindow: Duration)

  case class ListResponse(envelope: Array[EnvelopeObject])

  case class GetResponse(rainfall: Seq[Double], flow: Seq[Double], slope: Double,
                         intercept: Double, rSquare: Double, dates: Seq[Sessions.Session])

  case class ModelStatus(build: Int)

}

object ApiObjectsJsonProtocol extends DefaultJsonProtocol {

  import ApiObjects._

  /**
    * CreateEnvelopeParams formatter
    */
  implicit object CreateEnvelopeParamsFormat extends RootJsonFormat[CreateEnvelopeParams] {
    def write(params: CreateEnvelopeParams): JsObject = {
      JsObject(
        "type" -> JsString(params.modelType),
        "id" -> JsString(params.id)
      )
    }

    def read(value: JsValue): CreateEnvelopeParams = value match {
      case JsObject(request) =>
        val modelType = request.get("type").map(stringFromValue).getOrElse("envelope-v0")
        val id = request.get("id").map(stringFromValue).getOrElse(randomUUID().toString)
        CreateEnvelopeParams(modelType, id)
      case _ => CreateEnvelopeParams("envelope-v0", randomUUID().toString)
    }
  }

  /**
    * ModelCreatedResponse formatter
    */
  implicit object ModelCreatedResponseJsonFormat extends RootJsonFormat[ModelCreatedResponse] {
    def write(response: ModelCreatedResponse): JsObject = {
      JsObject(
        "id" -> JsString(response.id)
      )
    }

    def read(value: JsValue): ModelCreatedResponse = value match {
      case JsObject(response) =>
        val id = response.get("id").map(stringFromValue).getOrElse("")
        ModelCreatedResponse(id)
      case _ => ModelCreatedResponse("")
    }
  }

  /**
    * FitParams formatter
    */
  implicit object FitEnvelopeParamsFormat extends RootJsonFormat[FitEnvelopeParams] {

    def write(params: FitEnvelopeParams): JsObject = {
      JsObject(
        "flow" -> params.flow.toJson,
        "rainfall" -> params.rainfall.toJson,
        "stormIntensityWindow" -> params.stormIntensityWindow.toString.toJson,
        "flowIntensityWindow" -> params.flowIntensityWindow.toString.toJson,
        "dryDayWindow" -> params.dryDayWindow.toString.toJson,
        "minSessionWindow" -> params.minSessionWindow.toString.toJson,
        "maxSessionWindow" -> params.maxSessionWindow.toString.toJson,
        "flowBoundary" -> JsNumber(params.flowBoundary)
      )
    }

    def read(value: JsValue): FitEnvelopeParams = {
      val emptyTSP = TimeSeriesParams(LocalDateTime.now(), Duration.ZERO, Array())
      value match {

        case JsObject(x) =>
          FitEnvelopeParams(
            x.get("flow").map(_.convertTo[TimeSeriesParams]).getOrElse(emptyTSP)
            , x.get("rainfall").map(_.convertTo[TimeSeriesParams]).getOrElse(emptyTSP)
            , durationFromValue(x("dryDayWindow"))
            , durationFromValue(x("stormIntensityWindow"))
            , durationFromValue(x("flowIntensityWindow"))
            , if (x.contains("minSessionWindow")) durationFromValue(x("minSessionWindow"))
            else Duration.ZERO
            , if (x.contains("maxSessionWindow")) durationFromValue(x("maxSessionWindow"))
            else Duration.ZERO
            , x.get("flowBoundary").map(doubleFromValue).getOrElse(3.0)
          )
        case _ => FitEnvelopeParams(emptyTSP, emptyTSP, Duration.ZERO, Duration.ZERO, Duration.ZERO
          , Duration.ZERO, Duration.ZERO, Double.NaN)
      }
    }
  }

  /**
    * EnvelopeObject formatter
    */
  implicit object EnvelopeObjectFormat extends RootJsonFormat[ApiObjects.EnvelopeObject] {
    def write(envelope: ApiObjects.EnvelopeObject): JsObject = {
      JsObject(
        "id" -> JsString(envelope.id),
        "sessionWindow" -> envelope.sessionWindow.toString.toJson
      )
    }

    def read(json: JsValue): ApiObjects.EnvelopeObject = {

      json match {
        case JsObject(x) =>
          val id: String = stringFromValue(x.getOrElse("id", JsString("")))
          val sessionWindow = durationFromValue(x.getOrElse("sessionWindow", JsString("")))
          ApiObjects.EnvelopeObject(id, sessionWindow)
        case _ => ApiObjects.EnvelopeObject("", Duration.ZERO)
      }
    }
  }

  /**
    * Model List formatter
    */
  implicit object ListResponseFormat extends RootJsonFormat[ListResponse] {
    def write(status: ListResponse): JsObject = {
      JsObject(
        "envelope" -> JsArray(status.envelope.map {
          _.toJson
        }.toVector)
      )
    }

    def read(value: JsValue): ListResponse = {
      value match {
        case JsObject(x) => x.getOrElse("envelope", JsArray()) match {
          case JsArray(arr) =>
            ListResponse(arr.map(a => a.convertTo[ApiObjects.EnvelopeObject]).toArray)
          case _ => ListResponse(Array())
        }
        case _ => ListResponse(Array())
      }
    }
  }

  /**
    * Model Get formatter
    */
  implicit object GetResponseFormat extends RootJsonFormat[GetResponse] {
    def write(response: GetResponse): JsObject = {
      val storms = JsArray(response.rainfall.toVector.map(x => JsNumber(x)))
      val flow = JsArray(response.flow.toVector.map(x => JsNumber(x)))
      val dates = JsArray(response.dates.toVector.map(x => JsObject(
        "from" -> JsString(x.startIndex.toString),
        "to" -> JsString(x.endIndex.toString)
      )))
      val slope = JsNumber(response.slope)
      val intercept = JsNumber(response.intercept)
      val rSquare = JsNumber(response.rSquare)
      JsObject(
        "slope" -> slope,
        "intercept" -> intercept,
        "rsquare" -> rSquare,
        "storms" -> storms,
        "flow" -> flow,
        "dates" -> dates
      )
    }

    def read(value: JsValue): GetResponse = {

      value match {
        case JsObject(x) =>
          val rainfall = x.getOrElse("storms", JsArray()).convertTo[Array[Double]].toSeq
          val flow = x.getOrElse("flow", JsArray()).convertTo[Array[Double]].toSeq
          val slope = x.getOrElse("slope", JsNumber(0.0)).convertTo[Double]
          val intercept = x.getOrElse("intercept", JsNumber(0.0)).convertTo[Double]
          val rsquare = x.getOrElse("rsquare", JsNumber(0.0)).convertTo[Double]
          val dates = x.getOrElse("dates", JsArray()) match {
            case JsArray(arr) => arr.map {
              _ match {
                case JsObject(y) => Sessions.Session(
                  dtToInstant(timestampFromValue(y.getOrElse("from", JsString(""))))
                  , dtToInstant(timestampFromValue(y.getOrElse("to", JsString("")))))
                case _ => Sessions.Session(Instant.MIN, Instant.MIN)
              }
            }
            case _ => Seq()
          }
          GetResponse(rainfall, flow, slope, intercept, rsquare, dates)
        case _ => GetResponse(Seq(), Seq(), 0.0, 0.0, 0.0, Seq())
      }
    }
  }

  /**
    * ModelStatus formatter
    */
  implicit object ModelStatusFormat extends RootJsonFormat[ModelStatus] {
    def write(status: ModelStatus): JsObject = {
      JsObject(
        "build" -> JsNumber(status.build)
      )
    }

    def read(value: JsValue): ModelStatus = value match {
      case JsObject(request) =>
        val build = request.get("build").map(doubleFromValue).map(_.toInt).getOrElse(0)
        ModelStatus(build)
      case _ => ModelStatus(0)
    }
  }

}
