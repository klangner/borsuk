package carldata.borsuk

import java.io._

import org.slf4j.LoggerFactory


class ConfigParser {

  private val Log = LoggerFactory.getLogger(getClass.getName)

  def load(name: String): Option[String] = {
    var names = List[String]()
    var types = List[String]()
    new BufferedReader(new FileReader("config.yaml"))
      .lines()
      .forEach {
        x => {
          if (x.contains("name:")) names = names.+:(x.split(':')(1).trim)
          if (x.contains("type:")) types = types.+:(x.split(':')(1).trim)
        }
      }
    val models = names.zip(types)
    models.find(x => x._1 == name.trim)
      .map(x => x._2)
  }
}



