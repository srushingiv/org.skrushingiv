name := "skrushingiv"

organization := "org.skrushingiv"

scalaVersion := "2.10.5"

resolvers := Seq(
  Resolver.typesafeRepo("releases"))

libraryDependencies ++= Seq(
  "joda-time"          % "joda-time" % "2.8.1",
  "com.typesafe.play" %% "play-json" % "2.4.2"
)

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-Ywarn-dead-code",
  "-language:_",
  "-target:jvm-1.7",
  "-encoding", "UTF-8"
)
