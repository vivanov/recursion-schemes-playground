ThisBuild / scalaVersion := "2.13.10"

lazy val V = new {
  val cats             = "2.7.0"
}

lazy val `recursion-schemes-playground` = (project in file("."))
  .settings(
    name := "recursion-schemes-playground",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % V.cats,
      "org.typelevel" %% "cats-free" % V.cats
    )
  )
