package carldata.borsuk.iandi

import java.util.UUID.randomUUID

import carldata.borsuk.RDIIApiObjects.FitRDIIParams
import smile.regression.{RandomForest, randomForest}

class RDII(modelType: String) {
  val id: String = randomUUID().toString
  var model: Option[RandomForest] = None
  var buildNumber: Int = 0

  /** Mocked fit model */
  def fit(params: FitRDIIParams): Unit = {
    if (params.flow.nonEmpty) {
      val features: Array[Array[Double]] = params.flow.indices.map(_ % 24).map(i => Array(i.toDouble)).toArray
      model = Some(randomForest(features, params.flow, mtry = 1))
      buildNumber += 1
    }
  }

}
