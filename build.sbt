name := "simulation"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions ++= Seq("-feature")

libraryDependencies ++= Seq(
    // https://mvnrepository.com/artifact/org.apache.commons/commons-math3
    "org.apache.commons" % "commons-math3" % "3.6.1"
  )
