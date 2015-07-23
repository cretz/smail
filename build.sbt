resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

// EclipseKeys.configurations := Set(Compile, Test, IntegrationTest)

lazy val smail = (project in file(".")).
  configs(IntegrationTest).
  settings(Defaults.itSettings: _*).
  settings(
    name := "smail",
    version := "0.1.0",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq("-deprecation", "-feature"),
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.7",
    libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.12",
    libraryDependencies += "com.typesafe.akka" %% "akka-stream-experimental" % "1.0",
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0",
    libraryDependencies += "org.specs2" %% "specs2-core" % "3.6" % "it,test",
    libraryDependencies += "org.specs2" %% "specs2-junit" % "3.6" % "it,test",
    libraryDependencies += "com.sun.mail" % "javax.mail" % "1.5.3" % "it,test"
  )

initialize := {
  val required = "1.8"
  val current  = sys.props("java.specification.version")
  assert(current == required, s"Unsupported JDK: java.specification.version $current != $required")
}