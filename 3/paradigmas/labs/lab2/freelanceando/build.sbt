val ScalatraVersion = "2.7.0"

organization := ""

name := "freelanceando"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.10"

resolvers += Classpaths.typesafeReleases

libraryDependencies ++= Seq(
  "org.json4s"   %% "json4s-jackson" % "3.6.7",
  "org.scalatra" %% "scalatra" % ScalatraVersion,
  "org.scalatra" %% "scalatra-scalatest" % ScalatraVersion % "test",
  "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
  "org.eclipse.jetty" % "jetty-webapp" % "9.4.19.v20190610" % "container",
  "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided",
  "org.scalatra" %% "scalatra-json" % ScalatraVersion,
)

enablePlugins(SbtTwirl)
enablePlugins(ScalatraPlugin)
