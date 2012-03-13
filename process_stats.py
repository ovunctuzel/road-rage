#!/usr/bin/python2

import sys
import fileinput
from collections import defaultdict
import numpy as np
import pylab

if len(sys.argv) == 1:
  print "Give me stats logs!"
  sys.exit()

experiments = list()
# The first key for all of these is experiment.
# s1
lag_per_a = defaultdict(lambda: defaultdict(list))
lag_per_i = defaultdict(lambda: defaultdict(list))
all_lags = defaultdict(list)
# s2
avg_speed = defaultdict(list)
# s3
thruput_per_i = defaultdict(lambda: defaultdict(list))
# s4
# TODO find a clever way to pass in list of pairs, unpack, and wrap in a
# subroutine
active_per_time_x = defaultdict(list)
active_per_time_y = defaultdict(list)

# Grab input from everywhere first
exp = ""    # what experiment?
for line in fileinput.input():
  if fileinput.isfirstline():
    exp = line.strip()
    experiments.append(exp)
  elif line.startswith("s1"):
    nil, a, i, lag, time = line.strip().split(" ")
    lag = float(lag)
    lag_per_a[exp][a].append(lag)
    lag_per_i[exp][i].append(lag)
    all_lags[exp].append(lag)
  elif line.startswith("s2"):
    nil, a, time, dist = line.strip().split(" ")
    avg_speed[exp].append(float(dist) / float(time))
  elif line.startswith("s3"):
    nil, i, requests, entered, time = line.strip().split(" ")
    thruput_per_i[exp][i].append(float(entered) / float(requests))
  elif line.startswith("s4"):
    nil, time, cnt = line.strip().split(" ")
    active_per_time_x[exp].append(int(time))
    active_per_time_y[exp].append(int(cnt))

colors = ['b', 'r', 'g']

def hgram(transform, data, xlabel, yobject):
  pylab.xlabel(xlabel)
  pylab.ylabel("Number of " + yobject + " in this range")
  alpha = 1
  alpha_step = 0.9 / len(data)
  color = iter(colors)
  for exp, dat in data.items():
    ls = transform(dat)
    pylab.hist(ls, bins=50, histtype='stepfilled', label=exp, alpha=alpha,
               color=next(color))
    alpha -= alpha_step
    print exp + ": " + xlabel
    print "-" * len(xlabel)
    print "  Average =", np.average(ls)
    print "  Variance =", np.var(ls)
    print "  Min =", np.min(ls)
    print "  Max =", np.max(ls)
    print ""
  pylab.legend()
  pylab.show()

# Ways to process data
def identity(dat):
  return dat
def average(dat):
  return [np.mean(ls) for ls in dat.values()]

# Results from s1
hgram(average, lag_per_a, "Average lag per agent", "agents")
hgram(average, lag_per_i, "Average lag per intersection", "intersections")
hgram(identity, all_lags, "Average lag", "events")
# Results from s2
hgram(identity, avg_speed, "Average speed (m/s) of each agent", "agents")
# Results from s3
hgram(average, thruput_per_i,
      "Average throughput (entered / requests for some duration) per intersection",
      "intersections")
# Results from s4
pylab.xlabel("Time (seconds)")
pylab.ylabel("Number of active agents")
for exp in experiments:
  pylab.plot(active_per_time_x[exp], active_per_time_y[exp], label=exp)
pylab.legend()
pylab.show()
