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
    # TODO titles and y labels
    # TODO y axis should be more legible time
    boxplot2(read("unweighted_"), ylab="Time (s)", main="Title", col=colors)
    boxplot2(read("weighted_"), ylab="Time (s)", main="Title", col=colors)
    boxplot2(read("unweighted_savings_"), ylab="Time (s)", main="Title", col=colors)
    boxplot2(read("unweighted_savings_per_agent_"), ylab="Time (s)", main="Title", col=colors)
  }
}

# TODO one pdf report, or lots of images..
# TODO rm intermediate files, probably in the script.
