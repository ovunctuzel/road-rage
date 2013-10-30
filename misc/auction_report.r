require("gplots")
colors <- c("blue", "red", "green", "cyan", "orange", "purple", "coral")
cities = list()
cities["austin"] = "Austin"
cities["baton_rouge"] = "Baton Rouge"
cities["seattle"] = "Seattle"
cities["sf"] = "San Francisco"

concat <- function(s1, s2) {
  paste(s1, s2, sep="")
}

for (city in c("austin", "baton_rouge", "seattle", "sf")) {
  read <- function(fn) {
    read.table(concat(fn, city), header=TRUE)
  }

  if (file.exists(concat("unweighted_", city))) {
    # TODO x labels dont show up
    # TODO y axis should be more legible time
    boxplot2(
             read("unweighted_"), col=colors,
             ylab="Time (s)", main=concat("Unweighted trip times in ", cities[city]))
    boxplot2(
             read("weighted_"), col=colors,
             ylab="Time (s) * priority", main=concat("Weighted trip times in ", cities[city]))
    boxplot2(
             read("unweighted_savings_"), col=colors,
             ylab="Time savings (s)",
             main=concat("Unweighted trip time savings relative to FCFS in ", cities[city]))
    boxplot2(
             read("unweighted_savings_per_agent_"), col=colors,
             ylab="Time savings per agent (s)",
             main=concat("Unweighted trip time savings per agent relative to FCFS in ", cities[city]))
  }
}

# TODO one pdf report, or lots of images..
# TODO rm intermediate files, probably in the script.
