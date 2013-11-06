# This script is just for trip times

require("gplots")
require("RSQLite")

# Definitions
colors <- c("blue", "red", "green", "cyan", "orange", "purple", "coral")
city_names = list()
city_names["austin"] = "Austin"
city_names["baton_rouge"] = "Baton Rouge"
city_names["seattle"] = "Seattle"
city_names["sf"] = "San Francisco"

concat <- function(s1, s2) {
  paste(s1, s2, sep="")
}

# Read the raw data
raw_filenames <- Sys.glob(concat(commandArgs(trailingOnly=TRUE)[1], "/*trip_time*"))
raw_files <- Map(function(fn) { read.table(fn, header=TRUE) }, raw_filenames)
raw <- do.call(rbind, raw_files)
row.names(raw) <- seq(nrow(raw))

# Mangle the data into SQL
db <- dbConnect(dbDriver("SQLite"), "times.db")
dbWriteTable(db, "raw_times", raw)
cities <- dbGetQuery(db, "SELECT DISTINCT map FROM raw_times")

for (city in cities$map) {
  # TODO x labels dont show up. legend, colors?
  # TODO y axis should be more legible time
  # TODO macro to generate the query. each experiment, plus a transformation.
  filter <- concat(concat("FROM raw_times WHERE map='", city), "'")
  name <- city_names[city]

  unweighted <- dbGetQuery(db,
    concat(concat("SELECT SUM(fcfs), SUM(auctions_sysbids), SUM(auctions_no_sysbids),
                  SUM(equal_sysbids), SUM(equal_no_sysbids), SUM(fixed_sysbids),
                  SUM(fixed_no_sysbids) ", filter), " GROUP BY scenario"))
  boxplot2(unweighted, col=colors, ylab="Time (s)",
           main=concat("Unweighted trip times in ", name))

  weighted <- dbGetQuery(db,
    concat(concat("SELECT SUM(fcfs * priority), SUM(auctions_sysbids * priority),
                  SUM(auctions_no_sysbids * priority), SUM(equal_sysbids * priority),
                  SUM(equal_no_sysbids * priority), SUM(fixed_sysbids * priority),
                  SUM(fixed_no_sysbids * priority) ", filter), " GROUP BY scenario"))
  boxplot2(weighted, col=colors, ylab="Time (s) * priority",
           main=concat("Weighted trip times in ", name))

  unweighted_savings <- dbGetQuery(db,
    concat(concat("SELECT SUM(fcfs - auctions_sysbids), SUM(fcfs - auctions_no_sysbids),
                  SUM(fcfs - equal_sysbids), SUM(fcfs - equal_no_sysbids), SUM(fcfs - fixed_sysbids),
                  SUM(fcfs - fixed_no_sysbids) ", filter), " GROUP BY scenario"))
  boxplot2(unweighted_savings, col=colors, ylab="Time savings (s)",
           main=concat("Unweighted trip time savings relative to FCFS in ", name))

  unweighted_savings_per_agent <- dbGetQuery(db,
    concat("SELECT fcfs - auctions_sysbids, fcfs - auctions_no_sysbids, fcfs - equal_sysbids,
           fcfs - equal_no_sysbids, fcfs - fixed_sysbids, fcfs - fixed_no_sysbids ", filter))
  boxplot2(unweighted_savings_per_agent, col=colors, ylab="Time savings per agent (s)",
           main=concat("Unweighted trip time savings per agent relative to FCFS in ", name))
}
