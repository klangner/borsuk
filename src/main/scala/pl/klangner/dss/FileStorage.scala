package pl.klangner.dss

import org.slf4j.LoggerFactory

/**
  * Stores datasets into storage
  */
class FileStorage {

  private val Log = LoggerFactory.getLogger(getClass.getName)

  def add(dataset: String, features: Seq[String], target: String): Unit = {
    Log.info(s"Add datapoint to $dataset with features: $features and target: $target")
  }
}
