

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

EclipseKeys.configurations := Set(Compile, Test, IntegrationTest)

lazy val `scimap` = (project in file(".")).
  configs(IntegrationTest).
  settings(Defaults.itSettings: _*).
  settings(
    name := "scimap",
    version := "0.1.0",
    scalaVersion := "2.11.6",
    scalacOptions ++= Seq("-deprecation", "-feature"),
    libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11",
    libraryDependencies += "com.typesafe.akka" %% "akka-stream-experimental" % "1.0-RC3",
    libraryDependencies += "com.github.nscala-time" %% "nscala-time" % "2.0.0",
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0",
    libraryDependencies += "org.specs2" %% "specs2-core" % "3.6" % "it,test",
    libraryDependencies += "org.specs2" %% "specs2-junit" % "3.6" % "it,test",
    libraryDependencies += "com.sun.mail" % "javax.mail" % "1.5.3" % "it,test"
  )