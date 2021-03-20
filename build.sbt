import Dependencies._
import scala.sys.process._

ThisBuild / scalaVersion     := "2.13.2"
ThisBuild / version          := "0.1.0"

addCompilerPlugin("io.tryp" % "splain" % "0.5.7" cross CrossVersion.patch)

lazy val euismod = (project in file("."))
  .settings(
    libraryDependencies += scalaTest % Test,
    scalacOptions ++= Seq(
      "-Xplugin:kind-projector_2.10-0.6.0.jar",
      "-P:splain:all:true",
      /*"-Xlog-implicits", */
    )
  )

libraryDependencies += {
  val version = scalaBinaryVersion.value match {
    case "2.10" => "1.0.3"
    case _ ⇒ "2.2.0"
  }
  "com.lihaoyi" % "ammonite" % version % "test" cross CrossVersion.full
}
libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
)

sourceGenerators in Test += Def.task {
  val file = (sourceManaged in Test).value / "amm.scala"
  IO.write(file, """object amm extends App { ammonite.Main.main(args) }""")
  Seq(file)
}.taskValue

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

libraryDependencies += {
  val v =
    if (scalaVersion.value.replaceFirst(raw"\.(\d)$$",".0$1") <= "2.12.04") "0.4.1"
    else "0.5.7"
  ("io.tryp" %% "splain" % v cross CrossVersion.patch).withConfigurations(Some("plugin->default(compile)"))
}

