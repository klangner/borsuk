package carldata.borsuk

import carldata.borsuk.model.Model

/** List of registered models with meta information about it */
class ModelsDB(storage: FileStorage, configFile: String) {

  def getModel(name: String): Option[Model] = {
    None
  }

}



