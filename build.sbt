val CatsCoreVersion       = "2.2.0-M1"
val CatsMacrosVersion     = "2.1.1"
val CatsEffectVersion     = "2.1.3"
val CatsTagLessVersion    = "0.11"
val PureConfigVersion     = "0.12.0"

lazy val root = (project in file("."))
  .settings(
    organization := "com.vyunsergey",
    name := "cats-learn",
    version := "0.0.1",
    scalaVersion := "2.12.11",
    scalacOptions ++= scalaCompilerOptions,
    libraryDependencies ++= commonLibraryDependencies,
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
    addCompilerPlugin("org.spire-math" %% "kind-projector"     % "0.9.6"),
    addCompilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.2.4")
  )

lazy val commonLibraryDependencies = Seq(
  // Cats
  "org.typelevel"              %% "cats-core"           % CatsCoreVersion,
  "org.typelevel"              %% "cats-laws"           % CatsCoreVersion,
  "org.typelevel"              %% "cats-macros"         % CatsMacrosVersion,
  "org.typelevel"              %% "cats-effect"         % CatsEffectVersion,
  "org.typelevel"              %% "cats-effect-laws"    % CatsEffectVersion,
  "org.typelevel"              %% "cats-tagless-core"   % CatsTagLessVersion,
  "org.typelevel"              %% "cats-tagless-laws"   % CatsTagLessVersion,
  "org.typelevel"              %% "cats-tagless-macros" % CatsTagLessVersion,
  // PureConfig
  "com.github.pureconfig"      %% "pureconfig"          % PureConfigVersion
)

lazy val scalaCompilerOptions = Seq(
  "-deprecation",               // Emit warning and location for usages of deprecated APIs.
  "-encoding", "UTF-8",         // Specify character encoding used by source files.
  "-language:higherKinds",      // Allow higher-kinded types
  "-language:postfixOps",       // Allows operator syntax in postfix position (deprecated since Scala 2.10)
  "-feature",                   // Emit warning and location for usages of features that should be imported explicitly.
  "-Ypartial-unification",      // Enable partial unification in type constructor inference
  "-Ywarn-inaccessible",        // Warn about inaccessible types in method signatures
  "-Ywarn-infer-any",           // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override",    // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit",        // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen",       // Warn when numerics are widened.
  "-Ywarn-value-discard",       // Warn when non-Unit expression results are unused.
  "-Xfatal-warnings",           // Fail the compilation if there are any warnings
  "-Xfuture"                    // Turn on future language features
)
