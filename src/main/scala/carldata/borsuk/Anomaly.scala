package carldata.borsuk

import carldata.series.TimeSeries


object Anomaly {

  /**
    * Find anomalies and replace them interpolated value.
    **/
  def fixAnomalies(ts: TimeSeries[Double]): TimeSeries[Double] = {
    TimeSeries.interpolate(ts.filter(x => x._2 > 0), ts.resolution)
  }
}
