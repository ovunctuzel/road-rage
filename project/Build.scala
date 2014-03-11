import sbt._
import Keys._

object AortaBuild extends Build {
  val buildSettings = Defaults.defaultSettings ++ xerial.sbt.Pack.packSettings ++ Seq(
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.3",
    scalacOptions ++= Seq() // Seq("-unchecked", "-deprecation", "-Xlint")
  )

  lazy val aorta = Project(
    id = "aorta",
    base = file("."),
    settings = buildSettings ++ Seq(
      scalaSource in Compile := baseDirectory.value / "utexas",
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-swing" % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "org.jfree" % "jfreechart" % "1.0.15",
        "jline" % "jline" % "2.11"
      )
    )
  ).dependsOn(macros)

  lazy val macros = Project(
    id = "aorta-macros",
    base = file("macros"),
    settings = buildSettings ++ Seq(
      scalaSource in Compile := baseDirectory.value,
      autoCompilerPlugins := true,
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "org.scalamacros" % "quasiquotes" % "2.0.0-M3" cross CrossVersion.full,
        compilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M3" cross CrossVersion.full)
      )
    )
  )
}
