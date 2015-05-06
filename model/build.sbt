

import scalariform.formatter.preferences._

name := "diffusion"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.6"

//resolvers += "ISC-PIF Release" at "http://maven.iscpif.fr/public/"

//mainClass in (Compile, run) := Some("fr.iscpif.diffusion.Simulation")

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.5"

libraryDependencies += "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3-1"

//libraryDependencies += "fr.iscpif" %% "mgo" % "1.70-SNAPSHOT"


scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)

