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

      "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",

      //used for BigDecimal NumericType implementation
      "ch.obermuhlner" % "big-math" % "2.0.1",

      "org.jfree" % "jfreechart" % "1.5.0",
      "org.apache.xmlgraphics" % "batik-svggen" % "1.10",
      "org.apache.xmlgraphics" % "batik-dom" % "1.10",
      "org.apache.xmlgraphics" % "batik-awt-util" % "1.10",
      "org.apache.xmlgraphics" % "batik-util" % "1.10",
      "org.apache.xmlgraphics" % "batik-xml" % "1.10",
      "org.apache.xmlgraphics" % "batik-anim" % "1.10" % "test",


      //enable reflection
      //otherwise can't import scala.reflect.runtime
      //see https://stackoverflow.com/questions/25189608/cant-import-scala-reflect-runtime-universe
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
      )

//enable more warnings
scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")

//scalatest recommends unbuffered test output 
//see http://www.scalatest.org/user_guide/using_scalatest_with_sbt
logBuffered in Test := false

//make ScalaCheck give stack traces
//see https://stackoverflow.com/questions/24396407/how-to-display-entire-stack-trace-for-thrown-exceptions-from-scalacheck-tests
testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")
