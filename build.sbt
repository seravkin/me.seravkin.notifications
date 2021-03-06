name := "me.seravkin.notifications"

version := "0.4.7"

scalaVersion := "2.13.5"

assembly / assemblyJarName := "me-seravkin-notifications.jar"
assembly / assemblyMergeStrategy  := {
  case x if x.contains("io.netty.versions.properties") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}



enablePlugins(BuildInfoPlugin)

buildInfoKeys := Seq[BuildInfoKey](name, version)
buildInfoPackage := "me.seravkin.notifications"

lazy val compileScalastyle = taskKey[Unit]("compileScalastyle")

compileScalastyle := (Compile / scalastyle).toTask("").value

(Compile / compile) := ((Compile / compile) dependsOn compileScalastyle).value

scalacOptions ++= Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-value-discard"               // Warn when non-Unit expression results are unused.
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("staging")
)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
addCompilerPlugin("org.typelevel" % "kind-projector" % "0.12.0" cross CrossVersion.full)

val dependencies = new {
  val tgb4sV     = "5.0.0-0e30d39a-SNAPSHOT"
  val catsV      = "2.6.1"
  val catsEffV   = "2.5.1"
  val doobieV    = "0.13.2"
  val scalatestV = "3.2.9"
  val hikariCpV  = "3.2.0"
  val tsConfigV  = "1.3.2"
  val attoV      = "0.9.5"
  val catsMtlV   = "0.7.1"
  val pureCfgV   = "0.15.0"
  val flywayV    = "5.2.4"
  val sstpV      = "3.3.3"

  val tgBot4s = Seq(
    "com.bot4s" %% "telegram-core"          % tgb4sV,
    "com.softwaremill.sttp.client3" %% "core" % sstpV,
    "com.softwaremill.sttp.client3" %% "async-http-client-backend-cats-ce2" % sstpV
  )

  val cats = Seq(
    "org.typelevel" %% "cats-core"   % catsV,
    "org.typelevel" %% "cats-effect" % catsEffV,
    "org.typelevel" %% "cats-mtl-core" % catsMtlV
  )

  val parser = Seq(
    "org.tpolecat" %% "atto-core" % attoV
  )

  val pureconfig = Seq(
    "com.github.pureconfig" %% "pureconfig" % pureCfgV
  )

  val doobie = Seq(
    "org.tpolecat" %% "doobie-core"      % doobieV,
    "org.tpolecat" %% "doobie-hikari"    % doobieV,
    "org.tpolecat" %% "doobie-postgres"  % doobieV,
    "org.tpolecat" %% "doobie-scalatest" % doobieV % "test",
    "com.zaxxer"   %  "HikariCP"         % hikariCpV,
    "org.flywaydb" %  "flyway-core"      % flywayV
  )

  val config = Seq(
    "com.typesafe" % "config" % tsConfigV
  )

  val scalatest = Seq(
    "org.scalactic" %% "scalactic" % scalatestV,
    "org.scalatest" %% "scalatest" % scalatestV % "test"
  )
}

libraryDependencies ++= Seq(
  dependencies.tgBot4s,
  dependencies.cats,
  dependencies.parser,
  dependencies.doobie,
  dependencies.config,
  dependencies.pureconfig,
  dependencies.scalatest
).flatten