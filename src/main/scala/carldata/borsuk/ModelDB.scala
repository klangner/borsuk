package carldata.borsuk

import carldata.borsuk.model.Model

/** List of registered models with meta information about it */
class ModelDB(configFile: String) {

  def getModels[A](name: String): Seq[Model[A]] = {
    Seq()
  }

}



