package carldata.borsuk.rdiis

import java.nio.file.{Path, Paths}
import java.time.Duration

import akka.http.scaladsl.model.{HttpEntity, HttpResponse, MediaTypes, StatusCodes}
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import carldata.borsuk.Main
import carldata.borsuk.helper.{DateTimeHelper, PVCHelper}
import carldata.borsuk.rdiis.ApiObjects._
import carldata.borsuk.rdiis.ApiObjectsJsonProtocol._
import org.slf4j.LoggerFactory
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class RdiiApi {

  private val rdiisPath: String = "/borsuk_data/rdiis/"
  private val Log = LoggerFactory.getLogger(this.getClass.getName)

  def loadModel(modelType: String, id: String): Option[RDII] = {
    val rdii = new RDII(modelType, id)

    val path = Paths.get(rdiisPath + modelType)

    DateTimeHelper.logTime("PVCHelper.loadModel with path: " + path + " and id: " + id, PVCHelper.loadModel(path, id)).map {
      model =>
        val rDIIFileContent = model.content.parseJson.convertTo[RDIIFileContent](RDIIFileContentJsonProtocol.RDIIFileContentFormat)
        rdii.model = rDIIFileContent.rdiiResults
        rdii.buildNumber = rDIIFileContent.buildNumber
        rdii
    }
  }

  /**
    * Create new rdii model.
    * Use fit function to train this model
    */
  def create(params: CreateParams): StandardRoute = {
    val path: Path = Paths.get(rdiisPath + params.modelType)

    if (PVCHelper.modelExist(path, params.id)) {
      complete(StatusCodes.Conflict -> "Error: Model with this id already exist.")
    }
    else {
      val rdii = new RDII(params.modelType, params.id)
      rdii.save()
      val response = ModelCreatedResponse(params.id)

      complete(HttpResponse(
        StatusCodes.OK,
        entity = HttpEntity(MediaTypes.`application/json`, response.toJson.compactPrint)
      ))
    }
  }

  /** Fit the model to the training data */
  def fit(id: String, params: FitRDIIParams): StandardRoute = {
    DateTimeHelper.logTime("loadModel in RDIIFit type: " + params.modelType + " and id: " + id, loadModel(params.modelType, id)) match {
      case Some(model) =>
        Future {
          model.fit(params)
        }
        complete(StatusCodes.OK)

      case None =>
        complete(StatusCodes.NotFound)
    }
  }

  /** List the models of the training data */
  def list(id: String, sessionWindow: Duration, modelType: Option[String]): StandardRoute = {
    val mt = if (modelType.isDefined) modelType.get else "rdii-v0"
    DateTimeHelper.logTime("loadModel in RDIIList type: " + mt + " and id: " + id, loadModel(mt, id)) match {
      case Some(model) =>

        val response = ListResponse {
          model
            .list(sessionWindow)
            .map(x => ApiObjects.RDIIObject(x._1, x._2, x._3))
            .toArray
        }
        complete(HttpResponse(
          StatusCodes.OK,
          entity = HttpEntity(MediaTypes.`application/json`, response.toJson.compactPrint)
        ))

      case None =>
        complete(StatusCodes.NotFound)
    }
  }

  /** Get the model of the training data */
  def get(id: String, rdiiId: String, modelType: Option[String]): StandardRoute = {
    val mt = if (modelType.isDefined) modelType.get else "rdii-v0"
    DateTimeHelper.logTime("loadModel in RDIIGet type: " + mt + " and id: " + id, loadModel(mt, id)) match {
      case Some(model: RDII) =>
        model.get(rdiiId) match {
          case Some(rdii) =>
            val response: GetResponse = GetResponse(rdii._1, rdii._2, rdii._3, rdii._4, rdii._5, rdii._6)

            complete(HttpResponse(
              StatusCodes.OK,
              entity = HttpEntity(MediaTypes.`application/json`, response.toJson.compactPrint)
            ))
          case None =>
            complete(StatusCodes.NotFound)
        }

      case None =>
        complete(StatusCodes.NotFound)
    }
  }

  /**
    * Check model status. This function can be used to check if new revision of the model is trained
    * and the current model metric score.
    */
  def status(id: String, modelType: Option[String]): StandardRoute = {
    val mt = if (modelType.isDefined) modelType.get else "rdii-v0"
    DateTimeHelper.logTime("loadModel in RDIIStatus type: " + mt + " and id: " + id, loadModel(mt, id)) match {
      case Some(model) =>
        val status = ModelStatus(model.buildNumber)
        complete(HttpResponse(
          StatusCodes.OK,
          entity = HttpEntity(MediaTypes.`application/json`, status.toJson.compactPrint)
        ))

      case None =>
        complete(StatusCodes.NotFound)
    }
  }

}
