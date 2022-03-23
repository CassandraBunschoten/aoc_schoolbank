val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings( name                 := "aoc_schoolbank"
           , version              := "0.1.0"
           , scalaVersion         := scala3Version
           , libraryDependencies ++= Seq(
               "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
               "org.scalatest"  %% "scalatest"  % "3.2.11" % "test"
             )
           )
