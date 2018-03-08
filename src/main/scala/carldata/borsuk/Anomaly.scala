package carldata.borsuk

import carldata.series.Stats.meanAndVariance
import carldata.series.TimeSeries


class Anomaly(ts: TimeSeries[Double]) {

  /**
    * Find anomalies and replace them with mean value between previous and next point.
    * Return tuple with corrected TimeSeries and TimeSeries with anomalies
    **/
  def find: TimeSeries[Double] = {
    val diff = TimeSeries.differentiate(ts)
    val standard_deviation = Math.sqrt(meanAndVariance(diff.values).variance)
    val anomalies = diff.filter(x => x._2 > 3 * standard_deviation)
    val vs = ts.index.zip(ts.values.head +: ts.values).zip(ts.values).map { x =>
      if (anomalies.index.contains(x._1._1)) 0.5 * x._1._2 * x._2
      else x._2
    }

    new TimeSeries[Double](ts.index, vs)
  }
}
