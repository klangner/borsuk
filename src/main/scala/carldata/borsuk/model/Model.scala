package carldata.borsuk.model

/**
  * Trait for all Machine Learning model types
  */
trait Model {
  /** Add new sample to the model dataset */
  def addSample(str: String): Unit
}
