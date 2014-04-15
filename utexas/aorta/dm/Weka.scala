// AORTA is copyright (C) 2012 Dustin Carlino, Mike Depinet, and Piyush
// Khandelwal of UT Austin
// License: GNU GPL v2

package utexas.aorta.dm

import weka.core.{Attribute, FastVector, Instance, Instances}
import weka.classifiers.Evaluation
import weka.classifiers.bayes.NaiveBayes

import utexas.aorta.common.Util

class WekaClassifier(raw: ScrapedData) {
  // Setup
  private val labels = new FastVector(raw.labels.size)
  raw.labels.foreach(label => labels.addElement(label))
  private val attrib_ls = new Attribute("label", labels) :: raw.feature_names.map(new Attribute(_))
  private val attribs = new FastVector(attrib_ls.size)
  attrib_ls.foreach(a => attribs.addElement(a))

  // Train
  private val training_set = new Instances("aorta_relation", attribs, raw.data.size)
  training_set.setClassIndex(0)
  for (instance <- raw.data) {
    training_set.add(convert(instance))
  }

  private val model = build_model()

  private def build_model(): NaiveBayes = {
    val model = new NaiveBayes()
    model.buildClassifier(training_set)

    // Test
    val test = new Evaluation(training_set)
    test.evaluateModel(model, training_set)
    println(test.toSummaryString)

    return model
  }

  private def convert(instance: RawInstance): Instance = {
    val ex = new Instance(instance.features.size + 1)
    ex.setValue(attrib_ls.head, instance.label)
    for ((value, attrib) <- instance.features.zip(attrib_ls.tail)) {
      ex.setValue(attrib, value)
    }
    return ex
  }

  def classify(i: Instance) = raw.labels(model.classifyInstance(i).toInt)

  def find_anomalies() {
    val out = Util.writer("weird_osm_ids")
    val ids = raw.data.filter(i => {
      val ex = convert(i)
      ex.setDataset(training_set)
      val model_label = classify(ex)
      if (i.label != model_label) {
        //println(s"Labeled $i as $model_label")
      }
      i.label != model_label
    }).map(_.osm_id)
    out.int(ids.size)
    ids.foreach(id => out.string(id))
    out.done()
  }
}
