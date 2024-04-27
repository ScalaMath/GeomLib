ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

lazy val root = (project in file("."))
  .settings(
    name := "GeomLib"
  )

libraryDependencies += "io.github.scalamath" % "vecmatlib" % "3.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test