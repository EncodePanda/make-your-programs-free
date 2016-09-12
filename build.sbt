name := "make-your-programs-free"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-feature", "-language:higherKinds")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.6",
  "org.scalaz" %% "scalaz-concurrent" % "7.2.6",
  "org.slf4j" % "slf4j-api" % "1.7.21",
  "org.slf4j" % "slf4j-log4j12" % "1.7.21",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)
