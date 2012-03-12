#!/usr/bin/python2

from collections import defaultdict
import numpy as np
import pylab

# s1
lag_per_a = defaultdict(list)
lag_per_i = defaultdict(list)
all_lags = list()
# s2
avg_speed = list()
# s3
thruput_per_i = defaultdict(list)

input = open("stats_log")
for line in input:
  if line.startswith("s1"):
    nil, a, i, lag = line.strip().split(" ")
    lag = float(lag)
    lag_per_a[a].append(lag)
    lag_per_i[i].append(lag)
    all_lags.append(lag)
  elif line.startswith("s2"):
    nil, a, time, dist = line.strip().split(" ")
    avg_speed.append(float(dist) / float(time))
  elif line.startswith("s3"):
    nil, i, requests, entered, timespan = line.strip().split(" ")
    thruput_per_i[i].append(float(entered) / float(requests))
input.close()

def hgram(ls, xlabel, yobject):
  pylab.xlabel(xlabel)
  pylab.ylabel("Number of " + yobject + " in this range")
  pylab.hist(ls, bins=100, normed=1)
  print xlabel
  print "-" * len(xlabel)
  print "  Average =", np.average(ls)
  print "  Variance =", np.var(ls)
  print "  Min =", np.min(ls)
  print "  Max =", np.max(ls)
  print ""
  pylab.show()

# Results from s1
hgram([np.mean(ls) for ls in lag_per_a.values()], "Average lag per agent", "agents")
hgram([np.mean(ls) for ls in lag_per_i.values()], "Average lag per intersection",
      "intersections")
hgram(all_lags, "Average lag", "events")
# Results from s2
hgram(avg_speed, "Average speed (m/s) of each agent", "agents")
# Results from s3
hgram([np.mean(ls) for ls in thruput_per_i.values()],
      "Average throughput (entered / requests for some duration) per intersection",
      "intersections")
