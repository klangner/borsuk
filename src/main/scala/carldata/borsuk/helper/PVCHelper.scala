package carldata.borsuk.helper

import java.io._
import java.nio.file.{Files, Path, Paths}

import scala.collection.JavaConverters._

case class Model(modelType: String, id: String, content: String)

object PVCHelper {

  /**
    * Load all models from disk
    **/
  def loadModels(path: Path, f: Model => Unit): Unit = {
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
    *
    * @param path to directory with model
    * @param id   of model
    * @return
    */
  def modelExist(path: Path, id: String): Boolean = {
    Files.exists(Paths.get(path.toString + "/" + id))
  }

  /**
    * Delete model if exists
    *
    * @param path to directory with model
    * @param id   of model
    * @return
    */
  def deleteModel(path: Path, id: String): Unit = {
    if (modelExist(path, id)) {
      Files.delete(Paths.get(path.toString + "/" + id))
    }
  }

  /**
    * Load single model
    *
    * @param path to directory with model
    * @param id   of model
    * @return single model
    */
  def loadModel(path: Path, id: String): Option[Model] = {
    if (modelExist(path, id)) {
      val modelPath = Paths.get(path.toString + "/" + id)
      val content: String = new String(Files.readAllBytes(modelPath))
      val modelType: String = modelPath.getParent.getFileName.toString
      Some(Model(modelType, id, content))
    }
    else None
  }

  /**
    * Save model to disk
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

  /**
    * Save model to disk binary
    **/
  def saveModelBinary[T](path: Path, id: String, obj: AnyRef): Unit = {
    val filePath = Paths.get(path.toString, id)
    if (Files.notExists(path)) {
      Files.createDirectories(path)
    }

    val fos = new FileOutputStream(filePath.toString)
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(obj)
    oos.close()
    fos.close()
  }

  /**
    * Load single model binary
    *
    * @param path to directory with model
    * @param id   of model
    * @return single model
    */
  def loadModelBinary[T](path: Path, id: String): Option[T] = {
    if (modelExist(path, id)) {
      val modelPath = Paths.get(path.toString + "/" + id)

      val fis = new FileInputStream(modelPath.toString)
      val ois = new ObjectInputStream(fis)
      try {
        val obj = ois.readObject()
        Some(obj.asInstanceOf[T])
      }
      catch {
        case e: Exception => None
      }
      finally {
        ois.close()
        fis.close()
      }
    }
    else None
  }
}
