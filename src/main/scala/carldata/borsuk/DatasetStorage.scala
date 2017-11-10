package carldata.borsuk

import java.io._
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneOffset}

import org.slf4j.LoggerFactory

/**
  * Stores datasets onto storage
  */
class DatasetStorage(dataPath: String) {

  private val Log = LoggerFactory.getLogger(getClass.getName)

  /**
    * This function will:
    * - Create folder for each dataset
    * - Add data at the end of daily file,
    */
  def addDataPoint(datasetName: String, data: String): Unit = {
    new File(dataPath + "/" + datasetName).mkdirs()
    val now = LocalDateTime.now(ZoneOffset.UTC).format(DateTimeFormatter.ISO_DATE)
    val filePath = dataPath + "/" + datasetName + "/" + datasetName + "-" + now + ".json"

    val fw = new FileWriter(filePath, true)
    try {
      fw.write(data + "\n")
      StatsD.increment("data")
    } catch {
      case e: IOException =>
        Log.info(e.toString)
    }
    finally fw.close()
  }
}
