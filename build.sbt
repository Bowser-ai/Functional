ThisBuild / scalaVersion := "3.3.4"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "Functional",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.1.10",
      "dev.zio" %% "zio-test" % "2.1.10" % Test,
      "dev.zio" %% "zio-streams" % "2.1.10",
      "dev.zio" %% "zio-json" % "0.6.2"

    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
