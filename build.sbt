name := "aorta"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.0-M4"

// src/main/scala is too verbose
scalaSource in Compile := baseDirectory.value

// Don't eat my CPU
pollInterval := 5000

// TODO aliases that take args

// TODO managed dependencies
