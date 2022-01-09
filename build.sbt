name := "expl-scala"

version := "0.1"

scalaVersion := "2.13.7"
//???? Q: how to insert below in scala-parallel-collections, etc.?

 // https://medium.com/@awesomeorji/sbt-for-the-absolute-beginner-2-settings-and-tasks-6f3b00be1a81


 //“ThisBuild / scalaVersion := "2.13.0"


// Re options, see:
// - https://docs.scala-lang.org/overviews/compiler-options/index.html#Standard_Settings
// - https://docs.scala-lang.org/overviews/compiler-options/index.html#Advanced_Settings?

scalacOptions ++= Seq(
  "-deprecation",        // to get deprecation details
  "-feature",            // to get feature ~warning details
  "-unchecked",          // unclear; "where generated code depends on assumptions."
  "-Ymacro-annotations", // for macros like "@newtype" (apparently)

  "-Xlint:-unused,_",
  // -Xlint:unused sets -Wunused:imports,privates,locals,implicits.

  "-Wunused:nowarn",
  //"-Wunused:imports",
  "-Wunused:patvars",
  //"-Wunused:privates",
  //"-Wunused:locals",
  //"-Wunused:explicits", // explicit _parameters_
  "-Wunused:implicits",   // explicit _parameters_
  //"-Wunused:params",    // -Wunused:explicits,implicits.
  //"-Wunused:linted",    // -Xlint:unused.
  //??: -Wunused:synthetics?

  //??: -Vimplicits?


  )

// -explaintypes?
// -Werror / -Xfatal-warnings

/*
-Xsource


-Wdead-code      or -Ywarn-dead-code
-Wextra-implicit or -Ywarn-extra-implicit
-Wvalue-discard  or -Ywarn-value-discard
-Wnumeric-widen or -Ywarn-numeric-widen
-Woctal-literal or -Ywarn-octal-literal
-Wself-implicit or -Ywarn-self-implicit

-Wunused:WARNING1,WARNING2 or -Ywarn-unused:WARNING1,WARNING2:
  - imports   - Warn if an import selector is not referenced.
  - patvars   - Warn if a variable bound in a pattern is unused.
  - privates  - Warn if a private member is unused.
  - locals    - Warn if a local definition is unused.
  - explicits - Warn if an explicit parameter is unused.
  - implicits - Warn if an implicit parameter is unused.
  - params    - Enable -Wunused:explicits,implicits.
  - linted    - "-Xlint:unused." huh?


-Xlint:WARNING1,WARNING2 ("-WARNING" suppresses, "_" enables all others?):
  - adapted-args - Warn if an argument list is modified to match the receiver.
  - nullary-unit - Warn when nullary methods return Unit.
  - inaccessible - Warn about inaccessible types in method signatures.
  - nullary-override - Warn when non-nullary def f() overrides nullary def f.
  - infer-any - Warn when a type argument is inferred to be Any.
  - missing-interpolator - A string literal appears to be missing an interpolator id.
  - doc-detached - A Scaladoc comment appears to be detached from its element.
  - private-shadow - A private field (or class parameter) shadows a superclass field.
  - type-parameter-shadow - A local type parameter shadows a type already in scope.
  - poly-implicit-overload - Parameterized overloaded implicit methods are not visible as view bounds.
  - option-implicit - Option.apply used implicit view.
  - delayedinit-select - Selecting member of DelayedInit.
  - package-object-classes - Class or object defined in package object.
  - stars-align - Pattern sequence wildcard must align with sequence component.
  - constant - Evaluation of a constant arithmetic expression results in an error.
  - unused - Enable -Wunused:imports,privates,locals,implicits.
  - nonlocal-return - A return statement used an exception for flow control.
  - implicit-not-found - Check @implicitNotFound and @implicitAmbiguous messages.
  - serial - @SerialVersionUID on traits and non-serializable classes.
  - valpattern - Enable pattern checks in val definitions.
  - eta-zero - Warn on eta-expansion (rather than auto-application) of zero-ary method.
  - eta-sam - Warn on eta-expansion to meet a Java-defined functional interface that is not explicitly annotated with @FunctionalInterface.
  - deprecation - Enable linted deprecations.

- e.g.: -Wconf:msg=match may not be exhaustive:i
        -Wconf:cat=deprecation:ws,cat=feature:ws,cat=optimizer:ws
        https://www.scala-lang.org/2021/01/12/configuring-and-suppressing-warnings.html

*/


libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
  //"org.scala-lang.modules" %% "scala-parser-combinators" % <version>
  //"org.scala-lang.modules" %% "scala-parallel-collections" % "???",

  "org.scalatest" %% "scalatest" % "3.2.10" % Test,
  //"org.scalatest" %% "scalatest" % "3.2.10",  // Not just in src/test
  //"org.scalactic" %% "scalactic" % "3.2.10"

  "junit" % "junit" % "4.13.1" % Test,
  "org.specs2" %% "specs2-junit" % "4.11.0" % Test,

  //"org.specs2" %% "specs2-core_2.12" % "3.9.4" % Test,
  //"org.specs2" %% "specs2-junit_2.12" % "3.9.4" % Test,


  "org.json4s" % "json4s-core_2.13"   % "3.6.11",  // 3.7.x?, 4.x?
  "org.json4s" % "json4s-native_2.13" % "3.6.11",  // 3.7.x?, 4.x?
  "org.json4s" % "json4s-jackson_2.13" % "3.6.11",  // 3.7.x?, 4.x?
  //"org.json4s" % "json4s-ast_2.13" % "3.6.11",  // 3.7.x?, 4.x?



  // Stronger types:  newtypes and refine's refinement types
  "io.estatico" %% "newtype"             % "0.4.3",
  "eu.timepit"  %% "refined"                 % "0.9.28",
  //"eu.timepit"  %% "refined"                 % "0.9.28",/
  //"eu.timepit"  %% "refined-cats"            % "0.9.28", // optional
  //"eu.timepit"  %% "refined-eval"            % "0.9.28", // optional, JVM-only
  //"eu.timepit"  %% "refined-jsonpath"        % "0.9.28", // optional, JVM-only
  //"eu.timepit"  %% "refined-pureconfig"      % "0.9.28", // optional, JVM-only
  //"eu.timepit"  %% "refined-scalacheck"      % "0.9.28", // optional
  //"eu.timepit"  %% "refined-scalaz"          % "0.9.28", // optional
  //"eu.timepit"  %% "refined-scodec"          % "0.9.28", // optional
  //"eu.timepit"  %% "refined-scopt"           % "0.9.28", // optional
  //"eu.timepit"  %% "refined-shapeless"       % "0.9.28", // optional
  // try circe ~adapter

  "org.typelevel"  %% "cats-core"           % "2.1.0",
  // "org.typelevel"  %% "cats-effect"         % "2.1.0",

  "org.scalaz" %% "scalaz-core" % "7.3.5",


  "com.github.cb372" %% "cats-retry" % "2.1.0",
  // "org.typelevel"  %% "squants"  % "1.6.0",



  "org.tpolecat" %% "doobie-core" % "0.13.4",
  //"org.tpolecat" %% "doobie-postgres" % "0.13.4",
  //"org.tpolecat" %% "doobie-hikari" % "0.13.4",


  // https://github.com/typelevel/kind-projector:
   /*compilerPlugin(
     "org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full
   ),*/
   /*compilerPlugin(
     "org.augustjune" %% "context-applied" % "0.1.2"
   ),*/
  //"dev.profunktor" %% "console4cats"        % "0.8.1",
  //"org.manatki"    %% "derevo-cats"         % "0.10.5",
  //"org.manatki"    %% "derevo-cats-tagless" % "0.10.5",
  //"co.fs2"         %% "fs2-core"            % "2.2.2",
  //"com.olegpy"     %% "meow-mtl-core"       % "0.4.0",
  //"com.olegpy"     %% "meow-mtl-effects"    % "0.4.0",
  //"com.github.julien-truffaut" %% "monocle-core" % "2.0.1",
  //"com.github.julien-truffaut" %% "monocle-macro" % "2.0.1",

  //"org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

)


lazy val buildFirst = (project in file("buildFirst"))
//    .settings(publishArtifact := false)


lazy val root = (project in file("."))
    .aggregate(buildFirst)
    .dependsOn(buildFirst)
