#!/bin/bash
# Startup script each instance executes to setup packages and launch the script to run experiments.

# Install java and scala
sudo apt-get install -y openjdk-6-jre libjansi-java
wget http://scala-lang.org/files/archive/scala-2.10.2.deb
sudo dpkg -i scala-2.10.2.deb

# Grab the AORTA package
gsutil cp gs://aorta/aorta.tgz .  # TODO force...
mkdir aorta
cd aorta
tar xzf ../aorta.tgz
./recompile

# Run the experiment
./tools/analyze_routes gs://aorta/`hostname`-
