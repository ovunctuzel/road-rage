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

concat <- function(ls) {
  Reduce(function(a, b) { paste(a, b, sep="") }, ls)
}

if (file.exists("times.db")) {
  db <- dbConnect(dbDriver("SQLite"), "times.db")
} else {
  # Read the raw data
  raw_filenames <- Sys.glob(concat(c(commandArgs(trailingOnly=TRUE)[1], "/*trip_time*")))
  raw_files <- Map(function(fn) { read.table(fn, header=TRUE) }, raw_filenames)
  raw <- do.call(rbind, raw_files)
  row.names(raw) <- seq(nrow(raw))

  # Mangle the data into SQL
  db <- dbConnect(dbDriver("SQLite"), "times.db")
  dbWriteTable(db, "raw_times", raw)
  agent_count <- dbGetQuery(
    db, "SELECT scenario, MAX(agent) AS count FROM raw_times GROUP BY scenario")
  dbWriteTable(db, "agent_count", agent_count)
}

cities <- dbGetQuery(db, "SELECT DISTINCT map FROM raw_times")
for (city in cities$map) {
  # TODO y axis should be more legible time
  # TODO macro to generate the query. each experiment, plus a transformation.
  filter <- concat(c("FROM raw_times WHERE map='", city, "'"))
  # TODO if we were using sqldf, apply filter ONCE in this loop. compose operations!
  name <- city_names[city]
  count <- "SELECT count, scenario FROM agent_count"

  unweighted_sum <- "SELECT scenario, SUM(fcfs) AS fcfs, SUM(auctions_no_sysbids) AS an,
                     SUM(equal_no_sysbids) AS en, SUM(fixed_no_sysbids) AS fn,
                     SUM(auctions_sysbids) AS a, SUM(equal_sysbids) AS e, SUM(fixed_sysbids) AS f
                     FROM raw_times GROUP BY scenario"
  unweighted_query <- concat(c(
    "SELECT fcfs / count, an / count, en / count, fn / count, a / count, e / count, f / count
     FROM(", unweighted_sum, ") t1 INNER JOIN (", count, ") t2 ON t1.scenario == t2.scenario"))
  unweighted <- dbGetQuery(db, unweighted_query)
  boxplot2(unweighted, col=colors, xaxt="n", ylab="Time per agent (s / agent)",
           main=concat(c("Unweighted normalized trip times in ", name)))
  box()
  axis(side=1, at=1:7, tick=FALSE, line=1,
       labels=c("FCFS\n", "Auctions\n", "Equal\n", "Fixed\n", "Auctions\n+sysbid", "Equal\n+sysbid",
                "Fixed\n+sysbid"))

  #weighted <- dbGetQuery(db,
  #  concat(concat("SELECT SUM(fcfs * priority), SUM(auctions_sysbids * priority),
  #                SUM(auctions_no_sysbids * priority), SUM(equal_sysbids * priority),
  #                SUM(equal_no_sysbids * priority), SUM(fixed_sysbids * priority),
  #                SUM(fixed_no_sysbids * priority) ", filter), " GROUP BY scenario"))
  #boxplot2(weighted, col=colors, xlab="Time (s) * priority",
  #         main=concat("Weighted trip times in ", name), horizontal=TRUE, las=1,
  #         names=c("FCFS", "Auctions", "Equal", "Fixed", "Auctions+sysbids",
  #                 "Equal+sysbids", "Fixed+sysbids"))
#
#  unweighted_savings <- dbGetQuery(db,
#    concat(concat("SELECT SUM(fcfs - auctions_sysbids), SUM(fcfs - auctions_no_sysbids),
#                  SUM(fcfs - equal_sysbids), SUM(fcfs - equal_no_sysbids), SUM(fcfs - fixed_sysbids),
#                  SUM(fcfs - fixed_no_sysbids) ", filter), " GROUP BY scenario"))
#  boxplot2(unweighted_savings, col=colors, ylab="Time savings (s)",
#           main=concat("Unweighted trip time savings relative to FCFS in ", name))
#
#  unweighted_savings_per_agent <- dbGetQuery(db,
#    concat("SELECT fcfs - auctions_sysbids, fcfs - auctions_no_sysbids, fcfs - equal_sysbids,
#           fcfs - equal_no_sysbids, fcfs - fixed_sysbids, fcfs - fixed_no_sysbids ", filter))
#  boxplot2(unweighted_savings_per_agent, col=colors, ylab="Time savings per agent (s)",
#           main=concat("Unweighted trip time savings per agent relative to FCFS in ", name))
}
