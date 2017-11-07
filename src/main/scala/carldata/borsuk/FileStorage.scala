package carldata.borsuk

import java.io._
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneOffset}

import org.slf4j.LoggerFactory

/**
  * Stores datasets onto storage
  */
class FileStorage(dataPath: String) {

  private val Log = LoggerFactory.getLogger(getClass.getName)

  /**
    * This function will:
    * - Create folder for each dataset
    * - Add data at the end of daily file,
    */
  def add(dataset: String, data: String): Unit = {
    new File(dataPath + "/" + dataset).mkdirs()
    val now = LocalDateTime.now(ZoneOffset.UTC).format(DateTimeFormatter.ISO_DATE)
    val filePath = dataPath + "/" + dataset + "/" + dataset + "-" + now + ".csv"

    val fw = new FileWriter(filePath, true)
    try {
      fw.write(data + "\n")
    } catch {
      case e: IOException =>
        Log.info(e.toString)
    }
    finally fw.close()

    Log.info(s"Added datapoint to $dataset")
    Log.info(data)
  }
}
