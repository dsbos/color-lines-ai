name := "color-lines-ai"
version := "0.1"

// https://medium.com/@awesomeorji/sbt-for-the-absolute-beginner-2-settings-and-tasks-6f3b00be1a81

scalaVersion := "3.6.3"

// Re 3.x options, see:
// - https://docs.scala-lang.org/overviews/compiler-options/index.html
// - https://docs.scala-lang.org/scala3/guides/migration/options-intro.html
// - https://docs.scala-lang.org/scala3/guides/migration/options-new.html
// - https://docs.scala-lang.org/scala3/guides/migration/options-lookup.html



  /* Key/likely options (from "-help"; doesn't include -W/-X/etc. output):
               -Wconf  Configure compiler warnings.
              -Werror  Fail the compilation if there are any warnings.
         -deprecation  Emit warning and location for usages of deprecated APIs.
             -explain  Explain errors in more detail.
             -feature  Emit warning and location for usages of features that
                       should be imported explicitly.
              -nowarn  Silence all warnings.
         -print-lines  Show source code line numbers.
              -source  source version
                       Default 3.3
                       Choices : 3.0-migration, 3.0, 3.1, 3.2-migration, 3.2,
                       3.3-migration, 3.3, future-migration, future
           -unchecked  Enable additional warnings where generated code depends
                       on assumptions.
             -verbose  Output messages about what the compiler is doing.
             -version  Print product version and exit.
 */
  /*

  */
  /* Other likely options:
    - ?? TODO:  See when dead-code checking is implemented in Scala 3
    - ?? TODO:  Check other warnings/linting checks
  */

scalacOptions ++= Seq(
  // Language level/features:
  //"-source:future-migration", // (at 3.3.0:) warns for, e.g., old "_" in imports
  "-source:future",             // (at 3.3.0:) rejects import "_"; warns re "private[this]"
  "-language:strictEquality",

  "-Wunused:all",

  // Reporting level:
  "-deprecation",  // "Emit warning and location for usages of deprecated APIs."
  "-explain",      // "Explain errors in more detail."
  "-feature",      // "Emit warning and location for usages of features that should be imported explicitly."
  "-unchecked",    // "Enable additional warnings where generated code depends on assumptions."

  // "-help",  // Print a synopsis of standard options.
  // "-V",     // Print a synopsis of verbose options.
  // "-W",     // Print a synopsis of warning options.
  // "-X",     // Print a synopsis of advanced options.
  // "-Y",     //  Print a synopsis of private options.
  )


libraryDependencies ++= Seq(
  // Unit tests:
  "org.scalatest" %% "scalatest"  % "3.2.16" % Test,

  // Enumerations:
  // ?? TODO: TRY Scala 3 enums
  "com.beachape" %% "enumeratum"  % "1.7.2",

  // Stronger types (refine's refinement types):
  // ?? TODO:  See https://github.com/Iltotore/iron if refined doesn't re-add compile-time checks.
  "eu.timepit"  %% "refined"      % "0.11.0",

  // Other (Cats):
  "org.typelevel"  %% "cats-core" % "2.9.0",
)
