name := "me.seravkin.notifications"

version := "0.3.0"

scalaVersion := "2.12.4"

scalacOptions += "-Ypartial-unification"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("staging"),
  Resolver.bintrayRepo("seravkin","maven")
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

val http4sVersion = "0.18.0"

libraryDependencies ++= Seq(
  "info.mukel" %% "telegrambot4s" % "3.0.14",
  "org.typelevel" %% "cats-core" % "1.1.0",
  "org.typelevel" %% "cats-free" % "1.1.0",
  "org.typelevel" %% "cats-effect" % "1.0.0-RC2",
  "default" %% "me-seravkin-tg-adapter" % "0.1.1",
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "io.monix" %% "monix" % "3.0.0-M3",
  "org.scalactic" %% "scalactic" % "3.0.4",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test",
  "org.tpolecat" %% "doobie-core"      % "0.5.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.tpolecat" %% "doobie-h2"        % "0.5.0", // H2 driver 1.4.196 + type mappings.
  "org.tpolecat" %% "doobie-hikari"    % "0.5.0", // HikariCP transactor.
  "org.tpolecat" %% "doobie-postgres"  % "0.5.0", // Postgres driver 42.2.1 + type mappings.
  "org.tpolecat" %% "doobie-specs2"    % "0.5.0", // Specs2 support for typechecking statements.
  "org.tpolecat" %% "doobie-scalatest" % "0.5.0",  // ScalaTest support for typechecking statements.
  "com.h2database" % "h2" % "1.4.193",
  "com.zaxxer" % "HikariCP" % "2.7.7",
  // Optional for auto-derivation of JSON codecs
  "io.circe" %% "circe-generic" % "0.9.1",
  // Optional for string interpolation to JSON model
  "io.circe" %% "circe-literal" % "0.9.1",
  "com.typesafe" % "config" % "1.3.2"
)