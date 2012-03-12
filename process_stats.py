#!/usr/bin/python2

from collections import defaultdict
import numpy as np
import pylab

lag_per_a = defaultdict(list)
lag_per_i = defaultdict(list)
all_lags = list()

input = open("stats_log")
for line in input:
  if line.startswith("s1"):
    # Time wasting by an agent at each intersection
    nil, a, i, lag = line.strip().split(" ")
    lag = float(lag)
    lag_per_a[a].append(lag)
    lag_per_i[i].append(lag)
    all_lags.append(lag)
  # TODO handle s2, s3
input.close()

### Fiddle with s1 / time wasting

avg_lag_per_a = [np.mean(ls) for ls in lag_per_a.values()]
avg_lag_per_i = [np.mean(ls) for ls in lag_per_i.values()]

# TODO refactor this a bit?

pylab.xlabel("Average lag per agent")
pylab.ylabel("Number of agents in this range")
pylab.hist(avg_lag_per_a, bins=100, normed=1)
print "Average =", np.average(avg_lag_per_a), ", variance =", np.var(avg_lag_per_a)
pylab.show()

pylab.xlabel("Average lag per intersection")
pylab.ylabel("Number of intersections in this range")
pylab.hist(avg_lag_per_i, bins=100, normed=1)
pylab.show()

pylab.xlabel("Average lag")
pylab.ylabel("Number of events in this range")
pylab.hist(all_lags, bins=100, normed=1)
pylab.show()
