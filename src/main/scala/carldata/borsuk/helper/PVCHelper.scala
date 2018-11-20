package carldata.borsuk.helper

import java.nio.file.{Files, Path, Paths}

import scala.collection.JavaConverters._

case class Model(modelType: String, id: String, content: String)

object PVCHelper {

  /**
    * Load model from disk
    **/
  def loadModel(path: Path, f: Model => Unit): Unit = {
    if (Files.exists(path)) {
      val files = Files.walk(path).iterator().asScala.filter(Files.isRegularFile(_))
      files.map { model =>
        val modelPath = Paths.get(model.toString)
        val content: String = new String(Files.readAllBytes(modelPath))
        val modelType: String = modelPath.getParent.getFileName.toString
        val id: String = modelPath.getFileName.toString
        Model(modelType, id, content)
      }.foreach(f)
    }
  }

  /**
    * Save model from disk
    **/
  def saveModel(path: Path, model: Model): Path = {
    val filePath = Paths.get(path.toString, model.id)
    if (Files.exists(path)) {
      Files.write(filePath, model.content.getBytes)
    } else {
      Files.createDirectories(path)
      Files.write(filePath, model.content.getBytes)
    }
  }
}
