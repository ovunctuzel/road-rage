// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.dm

case class RawInstance(label: String, features: List[Double])
// min is inclusive, max is exclusive -- that way, bins are [0, num_bins)
case class FeatureSummary(feature: Int, min: Double, max: Double, num_bins: Int) {
  def transform(x: Double): Int = {
    if (x < min || x >= max) {
      // TODO or just cap... testing instance could exceed what we saw in training
      throw new IllegalArgumentException(s"$x not in [$min, $max]")
    }
    val normalized = (x - min) / (max - min)
    return math.floor(normalized * num_bins).toInt
  }
}

object Preprocessing {
  def summarize(instances: List[RawInstance], num_bins: Int): Seq[FeatureSummary] = {
    val mins = Array.fill(instances.head.features.size)(Double.PositiveInfinity)
    val maxs = Array.fill(instances.head.features.size)(Double.NegativeInfinity)
    for (instance <- instances) {
      for ((value, feature) <- instance.features.zipWithIndex) {
        mins(feature) = math.min(mins(feature), value)
        maxs(feature) = math.max(maxs(feature), value)
      }
    }
    return for (
      feature <- mins.indices
    ) yield FeatureSummary(feature, mins(feature), maxs(feature), num_bins)
  }
}
