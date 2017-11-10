package carldata.borsuk

import java.util.Properties

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import org.apache.kafka.clients.consumer.{CommitFailedException, ConsumerConfig, ConsumerRecords, KafkaConsumer}
import org.apache.kafka.common.serialization.StringDeserializer
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


object Main {

  private val Log = LoggerFactory.getLogger(Main.getClass.getName)

  implicit val system: ActorSystem = ActorSystem("borsuk")
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  val DATA_TOPIC = "borsuk"
  val POLL_TIMEOUT = 1000

  case class Params(dataPath: String, kafkaBroker: String, statsDHost: String)

  /** Routing */
  def route(datasetStorage: DatasetStorage): Route = {

    path("dataset" / Remaining) { dataset =>
      post {
        entity(as[String]) { data =>
          datasetStorage.addDataPoint(dataset, data)
          complete("ok")
        }
      }
    } ~ pathPrefix("static") {
      getFromDirectory("static")
    }
  }

  /** Parse application arguments */
  def parseArg(args: Array[String]): Params = {
    val dataPath = stringArg(args, "data-path", "data")
    val kafka = stringArg(args, "kafka", "localhost:9092")
    val statsDHost = stringArg(args, "statsd-host")
    Params(dataPath, kafka, statsDHost)
  }

  /** Parse single argument */
  def stringArg(args: Array[String], key: String, default: String = ""): String = {
    val name = "--" + key + "="
    args.find(_.contains(name)).map(_.substring(name.length)).getOrElse(default).trim
  }

  /** Kafka configuration */
  def kafkaConfig(brokers: String): Properties = {
    val strDeserializer = (new StringDeserializer).getClass.getName
    val props = new Properties()
    props.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, brokers)
    props.put(ConsumerConfig.GROUP_ID_CONFIG, "borsuk")
    props.put(ConsumerConfig.ENABLE_AUTO_COMMIT_CONFIG, "false")
    props.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, strDeserializer)
    props.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, strDeserializer)
    props
  }

  def main(args: Array[String]) {
    val params = parseArg(args)
    StatsD.init("borsuk", params.statsDHost)
    val storage = new DatasetStorage(params.dataPath)

    // Kafka consumer will run in separate thread
    Future(runKafkaConsumer(params.kafkaBroker, storage))
    // HTTP listener will run in main thread
    Log.info("Server started. Open http://localhost:7074/static/index.html")
    Await.result(Http().bindAndHandle(route(storage), "0.0.0.0", 7074), Duration.Inf)
  }

  /**
    * Try to predict number of records in the next batch
    */
  def runKafkaConsumer(kafkaBroker: String, datasetStorage: DatasetStorage): Unit = {
    val consumer = new KafkaConsumer[String, String](kafkaConfig(kafkaBroker))
    consumer.subscribe(List(DATA_TOPIC).asJava)

    while (true) {
      try {
        val batch: ConsumerRecords[String, String] = consumer.poll(POLL_TIMEOUT)
        batch.asScala.foreach { x =>
          datasetStorage.addDataPoint(x.key(), x.value())
        }
        consumer.commitSync()
      }
      catch {
        case e: CommitFailedException =>
          StatsD.increment("data.error.commit")
          Log.warn(e.toString)
        case e: Exception =>
          StatsD.increment("data.error")
          Log.error(e.toString)
      }
    }
    consumer.close()
  }
}