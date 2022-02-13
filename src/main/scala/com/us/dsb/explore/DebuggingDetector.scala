package com.us.dsb.explore

import scala.jdk.CollectionConverters._

object DebuggingDetector extends App {
  val bean = java.lang.management.ManagementFactory.getRuntimeMXBean
  val jvmArgs = bean.getInputArguments.asScala
  println("DebuggingDetector:  " + jvmArgs.mkString("JVM args:\n- ", "\n- ", ""))
  val startedWithDebugger =
    jvmArgs.exists {
      arg => arg.startsWith("-agentlib:jdwp") || arg.startsWith("-Xrunjdwp")}
  println("DebuggingDetector:  startedWithDebugger = " + startedWithDebugger)
}
