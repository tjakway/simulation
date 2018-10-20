name := "simulation"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions ++= Seq("-feature")

libraryDependencies ++= 
  Seq("org.slf4j" % "slf4j-parent" % "1.7.6",
      "ch.qos.logback"  %  "logback-classic"    % "1.2.1",
      "com.github.scopt" %% "scopt" % "3.5.0",

      //scalatest
      //see http://www.scalatest.org/install
      "org.scalatest" %% "scalatest" % "3.0.4" % "test",
      "org.scalactic" %% "scalactic" % "3.0.4" % "test",

      //enable reflection
      //otherwise can't import scala.reflect.runtime
      //see https://stackoverflow.com/questions/25189608/cant-import-scala-reflect-runtime-universe
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
      )

//enable more warnings
scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")
