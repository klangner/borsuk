name := "borsuk"

version := "0.3"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.1.5",
  "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.5",
  "com.github.haifengl" %% "smile-scala" % "1.5.1",
  "com.github.haifengl" % "smile-netlib" % "1.5.1",
  "io.github.carldata" %% "timeseries" % "0.6.6",
  "ch.megard" %% "akka-http-cors" % "0.2.2",
  "io.suzaku" %% "boopickle" % "1.3.0",


  // Log dependencies
  "org.slf4j" % "slf4j-api" % "1.7.25",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "de.siegmar" % "logback-gelf" % "1.0.4",
  "com.datadoghq" % "java-dogstatsd-client" % "2.3",
  // Test dependencies
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "com.typesafe.akka" %% "akka-http-testkit" % "10.1.5"
)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case "reference.conf" => MergeStrategy.concat
  case x => MergeStrategy.first
}

scalacOptions := Seq("-unchecked", "-deprecation")

assemblyJarName in assembly := "borsuk.jar"