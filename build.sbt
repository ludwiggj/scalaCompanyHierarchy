assemblyJarName in assembly := "PathFinder.jar"

mainClass in assembly := Some("chart.PathFinder")

name := "scalaCompanyHierarchy"

version := "1.0"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
"junit" % "junit" % "4.11" % "test",
"org.scalatest" %% "scalatest" % "2.2.1" % "test"
)