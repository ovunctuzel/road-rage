#!/bin/bash
# Startup script each instance executes to setup packages and launch the script to run experiments.

GS=gs://aorta/`hostname`-

# Install java and scala
sudo apt-get install -y openjdk-6-jre libjansi-java
wget http://scala-lang.org/files/archive/scala-2.10.2.deb
sudo dpkg -i scala-2.10.2.deb

# Grab the AORTA package
gsutil cp gs://aorta/aorta.tgz .  # TODO force...
mkdir aorta
cd aorta
tar xzf ../aorta.tgz
./tools/cloud/upload_gs.sh ${GS}-status 'Compiling'
./recompile

# Run the experiment
./tools/analyze_routes $GS
# TODO do it multiple times to amortize compilation time? can't overwrite results

# When we're done, shutdown
./tools/cloud/upload_gs.sh ${GS}-status 'Done'
gcutil deleteinstance --force `hostname`
