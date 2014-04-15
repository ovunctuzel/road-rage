// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.dm

import scala.collection.mutable

// The feature values have been discretized; the possible values are [0, bins)
case class LabeledInstance(label: String, features: List[Int]) {
  def for_test = UnlabeledInstance(features)
}
case class UnlabeledInstance(features: List[Int])

class NaiveBayesClassifier(labels: Set[String], bins: Int) {
  private var priors: Map[String, Double] = Map()
  private val features = new mutable.HashMap[(String, Int, Int), Double]()

  def train(training_data: List[LabeledInstance], validation_data: List[LabeledInstance]) {
    // the key is just the label
    val prior_counts = new mutable.HashMap[String, Int]().withDefaultValue(0)
    // the key is (label, feature idx, bin value)
    val feature_counts = new mutable.HashMap[(String, Int, Int), Int]().withDefaultValue(0)

    for (instance <- training_data) {
      prior_counts(instance.label) += 1
      for ((bin, feature) <- instance.features.zipWithIndex) {
        feature_counts((instance.label, feature, bin)) += 1
      }
    }
    priors = normalize(prior_counts.toMap)
    val num_features = training_data.head.features.size
    for (label <- labels) {
      for (feature <- Range(0, num_features)) {
        // denominator is the same for every feature
        val denominator = Range(0, bins).map(value => feature_counts((label, feature, value))).sum
        for (value <- Range(0, bins)) {
          val key = (label, feature, value)
          // TODO add different laplace smoothing parameters k, choose the best using validation_data
          features(key) = feature_counts(key) / denominator
        }
      }
    }
  }

  def classify(instance: UnlabeledInstance): String = {
    return labels.maxBy(label => posterior(instance, label))
  }

  // Returns log(p(instance and class = label))
  private def posterior(instance: UnlabeledInstance, label: String) =
    math.log(priors(label)) + instance.features.zipWithIndex.map({
      case (bin, feature) => math.log(features((label, feature, bin)))
    }).sum

  def summarize(tests: List[LabeledInstance]) {
    println(s"Number of bins = $bins")
    for ((label, prior) <- priors.toList.sortBy(_._2).reverse) {
      println(f"$label has a prior of ${prior * 100}%.2f%%")
    }
    println("")

    var num_correct = 0
    for (instance <- tests) {
      if (classify(instance.for_test) == instance.label) {
        num_correct += 1
      }
    }
    println(s"$num_correct / ${tests.size} correct: accuracy is ${num_correct.toDouble / tests.size}")
  }

  private def normalize[K](m: Map[K, Int]): Map[K, Double] = {
    val sum = m.values.sum
    return m.mapValues(count => count.toDouble / sum)
  }
}
