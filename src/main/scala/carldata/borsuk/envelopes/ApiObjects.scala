package carldata.borsuk.envelopes

import java.time.{Duration, LocalDateTime}
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

  case class FitEnvelopeParams(flow: TimeSeriesParams, rainfall: TimeSeriesParams, dryDayWindow: Duration,
                               stormIntensityWindow: Duration, flowIntensityWindow: Duration,
                               minSessionWindow: Duration, maxSessionWindow: Duration)

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
        val modelType = request.get("type").map(stringFromValue).getOrElse("daily-pattern-v0")
        val id = request.get("id").map(stringFromValue).getOrElse(randomUUID().toString)
        CreateEnvelopeParams(modelType, id)
      case _ => CreateEnvelopeParams("daily-pattern-v0", randomUUID().toString)
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
    val emptyTSP = TimeSeriesParams(LocalDateTime.now(), Duration.ZERO, Array())

    def write(params: FitEnvelopeParams): JsObject = {
      JsObject(
        "flow" -> params.flow.toJson,
        "rainfall" -> params.rainfall.toJson,
        "stormIntensityWindow" -> params.stormIntensityWindow.toString.toJson,
        "flowIntensityWindow" -> params.flowIntensityWindow.toString.toJson,
        "dryDayWindow" -> params.dryDayWindow.toString.toJson,
        "minSessionWindow" -> params.minSessionWindow.toString.toJson,
        "maxSessionWindow" -> params.maxSessionWindow.toString.toJson
      )
    }

    def read(value: JsValue): FitEnvelopeParams = value match {

      case JsObject(x) =>
        FitEnvelopeParams(
          x.get("flow").map(_.convertTo[TimeSeriesParams]).getOrElse(emptyTSP),
          x.get("rainfall").map(_.convertTo[TimeSeriesParams]).getOrElse(emptyTSP),
          Duration.parse(x("dryDayWindow").asInstanceOf[JsString].value),
          Duration.parse(x("stormIntensityWindow").asInstanceOf[JsString].value),
          Duration.parse(x("flowIntensityWindow").asInstanceOf[JsString].value),
          if (x.contains("minSessionWindow")) Duration.parse(x("minSessionWindow").asInstanceOf[JsString].value)
          else Duration.ZERO,
          if (x.contains("maxSessionWindow")) Duration.parse(x("maxSessionWindow").asInstanceOf[JsString].value)
          else Duration.ZERO
        )
      case _ => FitEnvelopeParams(emptyTSP, emptyTSP, Duration.ZERO, Duration.ZERO, Duration.ZERO, Duration.ZERO, Duration.ZERO)
    }
  }

  /**
    * EnvelopeObject formatter
    */
  implicit object EnvelopeObjectFormat extends RootJsonFormat[EnvelopeObject] {
    def write(envelope: EnvelopeObject): JsObject = {
      JsObject(
        "id" -> envelope.id.toJson,
        "sessionWindow" -> envelope.sessionWindow.toString.toJson
      )
    }

    def read(json: JsValue): EnvelopeObject = {
      val fields = json.asJsObject.fields
      val id: String = fields("id").toString
      val sessionWindow = Duration.parse(fields("sessionWindow").asInstanceOf[JsString].value)

      EnvelopeObject(id, sessionWindow)
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
      value.asJsObject().fields("envelope") match {
        case JsArray(arr) =>
          ListResponse(arr.map { a =>
            a.convertTo[EnvelopeObject]
          }.toArray)
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
      val fields = value.asJsObject.fields

      val rainfall = fields("rainfall").convertTo[Array[Double]]
      val flow = fields("flow").convertTo[Array[Double]]
      val slope = fields("slope").convertTo[Double]
      val intercept = fields("intercept").convertTo[Double]
      val rsquare = fields("rsquare").convertTo[Double]
      val dates = fields("dates").asInstanceOf[JsArray].elements
        .map(obj => Sessions.Session(dtToInstant(timestampFromValue(obj.asJsObject.fields("from"))),
          dtToInstant(timestampFromValue(obj.asJsObject.fields("to")))))
      GetResponse(rainfall, flow, slope, intercept, rsquare, dates)
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
