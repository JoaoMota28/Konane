ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.2"

lazy val root = (project in file("."))
  .settings(
    name := "LM23_JoaoMota128782_JoaoNunes128791_RodrigoSilva105447"
  )

libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
libraryDependencies += "org.openjfx" % "javafx-base" % "25.0.2"
libraryDependencies += "org.openjfx" % "javafx-controls" % "25.0.2"
libraryDependencies += "org.openjfx" % "javafx-fxml" % "25.0.2"
libraryDependencies += "org.openjfx" % "javafx-graphics" % "25.0.2"
