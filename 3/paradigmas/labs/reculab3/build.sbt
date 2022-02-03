name := "Lab 3 Reactive"

version := "0.1-SNAPSHOT"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.6.5",
  "com.typesafe.akka" %% "akka-http" % "10.1.11",
  "com.typesafe.akka" %% "akka-stream" % "2.6.5",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.11",
  "org.dispatchhttp" %% "dispatch-core" % "1.2.0"
)
