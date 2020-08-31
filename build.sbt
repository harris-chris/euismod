import Dependencies._

ThisBuild / scalaVersion     := "2.13.2"
ThisBuild / version          := "0.1.0"

lazy val sheetRef = ProjectRef(
  uri("git://github.com/petri-dev/sheet-ref.git#master"), "SheetRef")

object gitDependencies {
  lazy val sportDate = ProjectRef(
    uri("git@github.com:chrisharriscjh/sport-date.git#master"), "sportDate"
  )
}

lazy val root = (project in file("."))
  .settings(
    name := "petriarray",
    libraryDependencies += scalaTest % Test
  )
  .dependsOn(gitDependencies.sportDate)

def removegit = Command.command("removegit"){state =>
  val home = sys.env("HOME")
  val k = ("rm -rf "+ home + "/.sbt/1.0/staging/").!
  state
}

commands ++= Seq(removegit)
