// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.dm

import weka.core.{Attribute, FastVector, Instance, Instances}
import weka.classifiers.Evaluation
import weka.classifiers.bayes.NaiveBayes

class WekaClassifier(raw: ScrapedData) {
  private val training_set = build_training_data()
  private val model = build_model()

  private def build_training_data(): Instances = {
    // Setup
    val labels = new FastVector(raw.labels.size)
    raw.labels.foreach(label => labels.addElement(label))
    val attrib_ls = new Attribute("label", labels) :: raw.feature_names.map(new Attribute(_))
    val attribs = new FastVector(attrib_ls.size)
    attrib_ls.foreach(a => attribs.addElement(a))

    // Train
    val training_set = new Instances("osm_relation", attribs, raw.data.size)
    training_set.setClassIndex(0)
    for (instance <- raw.data) {
      val ex = new Instance(instance.features.size + 1)
      ex.setValue(attrib_ls.head, instance.label)
      for ((value, attrib) <- instance.features.zip(attrib_ls.tail)) {
        ex.setValue(attrib, value)
      }
      training_set.add(ex)
    }
    return training_set
  }

  private def build_model(): NaiveBayes = {
    val model = new NaiveBayes()
    model.buildClassifier(training_set)

    // Test
    val test = new Evaluation(training_set)
    test.evaluateModel(model, training_set)
    println(test.toSummaryString)

    return model
  }

  def classify(i: Instance) = raw.labels(model.classifyInstance(i).toInt)

  def find_anomalies() {
    for (idx <- Range(0, training_set.numInstances)) {
      val i = training_set.instance(idx)
      val model_label = classify(i)
      val true_label = i.classAttribute.value(i.classValue.toInt)

      if (model_label != true_label) {
        println(s"Labeled $i as $model_label")
      }
    }
  }
}
