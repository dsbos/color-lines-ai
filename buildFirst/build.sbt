name := "buildFirst subproject"

//???? clean up (just created subproject): factor out commonality


version := "0.1"

scalaVersion := "2.13.7"

// https://medium.com/@awesomeorji/sbt-for-the-absolute-beginner-2-settings-and-tasks-6f3b00be1a81



//“ThisBuild / scalaVersion := "2.13.0"


scalacOptions += "-deprecation"
scalacOptions += "-Ymacro-annotations"
scalacOptions += "-feature"

//scalacOptions += "-Ymacro-debug-verbose"  //????
//scalacOptions += "-Yshow-trees-stringified"  //????



libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.9" % Test,

  // Stronger types:  newtypes and refine's refinement types
  "io.estatico" %% "newtype"             % "0.4.3",
  "eu.timepit"  %% "refined"                 % "0.9.28",
  //"eu.timepit"  %% "refined-cats"            % "0.9.28", // optional
  //"eu.timepit"  %% "refined-eval"            % "0.9.28", // optional, JVM-only
  //"eu.timepit"  %% "refined-jsonpath"        % "0.9.28", // optional, JVM-only
  //"eu.timepit"  %% "refined-pureconfig"      % "0.9.28", // optional, JVM-only
  //"eu.timepit"  %% "refined-scalacheck"      % "0.9.28", // optional
  //"eu.timepit"  %% "refined-scalaz"          % "0.9.28", // optional
  //"eu.timepit"  %% "refined-scodec"          % "0.9.28", // optional
  //"eu.timepit"  %% "refined-scopt"           % "0.9.28", // optional
  //"eu.timepit"  %% "refined-shapeless"       % "0.9.28",  // optional


  "org.typelevel"  %% "cats-core"           % "2.1.0",
  "org.typelevel"  %% "cats-effect"         % "2.1.0",


  "com.github.cb372" %% "cats-retry" % "2.1.0",
  "org.typelevel"  %% "squants"  % "1.6.0",

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

