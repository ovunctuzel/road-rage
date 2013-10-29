require("gplots")
colors <- c("blue", "red", "green", "cyan", "orange", "purple", "coral")

# TODO foreach city, metric...
utimes <- read.table("unweighted_austin", header=TRUE)
boxplot2(utimes, ylab="Time (s)", main="Title", col=colors)
# TODO y axis should be more legible time
