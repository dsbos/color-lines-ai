package com.us.dsb.explore

import scala.jdk.CollectionConverters._

object DebuggingDetector extends App {
  val bean = java.lang.management.ManagementFactory.getRuntimeMXBean
  val jvmArgs = bean.getInputArguments.asScala
  println(jvmArgs.mkString("JVM args:\n- ", "\n- ", ""))
  val startedWithDebugger = jvmArgs.exists(_.startsWith("-agentlib:jdwp"))
  println("DebuggingDetector:  startedWithDebugger = " + startedWithDebugger)
}
