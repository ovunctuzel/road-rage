name := "aorta"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.0-M4"

// src/main/scala is too verbose
scalaSource in Compile := baseDirectory.value

// Whine about everything?
//scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlint")

// TODO aliases that take args

// TODO managed dependencies for everything
libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11.0-M4"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0-RC3"

libraryDependencies += "com.xeiam.xchart" % "xchart" % "2.3.0"

// Be able to collect all dependencies
packSettings
