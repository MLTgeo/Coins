name := "diffusion"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.3"

mainClass in (Compile, run) := Some("fr.iscpif.diffusion.Simulation")

libraryDependencies += "com.github.scopt" % "scopt_2.10.0-RC2" % "2.1.0"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2"

scalariformSettings

//scalacOptions in (Compile, doc) ++=
//  Opts.doc.sourceUrl("https://forge.iscpif.fr/projects/coin/repository/revisions/master/show/model/src/main/scala/€{TPL_OWNER}.€{TPL_NAME}.scala")

