name := "Pet Rescue Solver"

version := "0.1"

scalaVersion := "2.10.2"

resolvers += "typesafe-repo" at "http://repo.typesafe.com/"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

libraryDependencies += "junit" % "junit" % "4.5" % "test"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.2.0"

//libraryDependencies += "com.typesafe.atmos" % "trace-akka-2.2.0_2.10" % "1.2.0"

atmosSettings
