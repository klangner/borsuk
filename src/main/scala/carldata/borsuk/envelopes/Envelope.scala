package carldata.borsuk.envelopes

import ApiObjects.FitEnvelopeParams
import java.time.Duration
import scala.collection.immutable.HashMap

case class EnvelopeObject(sessionWindow: Duration)

class Envelope(modelType: String) {
  var model = HashMap.empty[String, EnvelopeObject]
  var buildNumber: Int = 0

  def fit(params: FitEnvelopeParams) = {
    model = HashMap(List[(String,EnvelopeObject)](("1", EnvelopeObject(Duration.ofMinutes(5)))) : _*)
    buildNumber += 1
  }

}