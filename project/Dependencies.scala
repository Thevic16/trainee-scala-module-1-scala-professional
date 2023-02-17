import sbt._

object Version {
  val scalaVer     = "2.13.4"
  val scalaTest    = "3.2.2"
  val playJson     = "2.9.2"
}

object Library {
  val scalaTest    = "org.scalatest"          %% "scalatest"                % Version.scalaTest
  val playJson     = "com.typesafe.play"      %% "play-json"                % Version.playJson
}

object Dependencies {

  import Library._

  val dependencies = List(
    playJson,
    scalaTest % "test"
  )
}
