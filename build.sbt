//name := "expl-scala"
name := "color-lines-ai"

version := "0.1"

scalaVersion := "2.13.10"
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
  // Unit tests:
  "org.scalatest" %% "scalatest" % "3.2.14" % Test,  // Currently used; UPDATE

  // Enumerations
  "com.beachape" %% "enumeratum" % "1.7.0",  // Currently used; UPDATE; TRY Scala 3 enums


  // Stronger types:  newtypes and refine's refinement types
  "io.estatico" %% "newtype"     % "0.4.3", // Currently used; UPDATE; TRY Scala 3 opaque types
  "eu.timepit"  %% "refined"     % "0.10.1", // Currently used; UPDATE

  // Other (Cats):
  "org.typelevel"  %% "cats-core"           % "2.1.0", // Currently used; UPDATE

)


