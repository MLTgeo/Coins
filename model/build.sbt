name := "coin"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.2"

//commentaire
//mainClass in (Compile, run) := Some("fr.iscpif.coin.circular.Simulation")
mainClass in (Compile, run) := Some("fr.iscpif.coin.holidays.Simulation")

libraryDependencies += "com.github.scopt" % "scopt_2.10.0-RC2" % "2.1.0"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2"

scalariformSettings

