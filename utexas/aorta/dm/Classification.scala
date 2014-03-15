// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.dm

abstract class Classifier(possible_labels: Set[String]) {
  // list of Counters (mapping feature description to count?)
  def train(training_data, validation_data)

  def classify(datum): String
}


// counter is a map from string to double... or to int? count?
// argmax, adding two counters, normalizing
