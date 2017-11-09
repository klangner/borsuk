package carldata.borsuk.model

/**
  * Trait for all Machine Learning model types
  * This contains model helpers which are implemented by different model kinds.
  */
trait Model[A] {

  /** Update model from given data */
  def update(data: String): Unit

  /** Predict value based on given feature data*/
  def predict(data: String): A
}
