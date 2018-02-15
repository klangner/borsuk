package carldata.borsuk

import akka.http.scaladsl.server.StandardRoute
import akka.http.scaladsl.server.Directives.complete

class Prediction {
  def find(): StandardRoute = {
    val csv =
      s"""|time,value
          |2017-01-12T12:00,3
          |2017-01-12T12:05,3
      """.stripMargin
    complete(csv)
  }
}
