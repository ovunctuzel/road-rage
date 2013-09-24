#!/bin/bash
# Script to shutdown all instances

for i in `ec2din | grep running | cut -f2`
do
  ec2kill $i
done
