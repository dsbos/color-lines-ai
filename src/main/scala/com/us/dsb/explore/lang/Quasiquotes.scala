package com.us.dsb.explore.lang

object Quasiquotes extends App {

  locally {
    import scala.reflect.runtime.currentMirror
    import scala.reflect.runtime.universe._
    import scala.tools.reflect.ToolBox
    val toolbox = currentMirror.mkToolBox()
    //showCode
    //showRaw

    import scala.util.Try

    locally {
      val x11: Tree = q"""println("<printed string>"); "<returned string>""""
      println("- quote x11:\n" + x11)

      println("- showCode(x11) (tree as source (usually)):\n"
                  + showCode(x11))

      println("- showRaw(x11) (tree as Block(List(Ident(..., etc.):\n"
                  + showRaw(x11))

      val x12: () => Any = toolbox.compile(x11)
      println("- toolbox.compile(x11) x12:\n" + x12)

      val x13: Any = {
        println("- execution val x13 = x12.apply:")
        x12.apply
      }
      println("- returned value x13:\n" + x13)

      // shows "scala.Predef.println" (vs. just "println" from q"...")
      println("- toolbox.typecheck(x11):\n" + toolbox.typecheck(x11))
    }
    println("========================================")
    locally {
      val x11: Tree = q"""noSuchSymbol"""
      println("- quote x11:\n" + x11)

      val x12: Try[_] = Try(toolbox.compile(x11))
      println("- Try(toolbox.compile(x11)) x12:\n" + x12)

      println("- x12.failed.get:\n" + x12.failed.get)
      println("- x12.failed.get.getMessage:\n" + x12.failed.get.getMessage)
      println("- x12.failed.get.getCause:\n" + x12.failed.get.getCause)

      // shows "scala.Predef.println" (vs. just "println" from q"...")
      println("- Try(toolbox.typecheck(x11)):\n" + Try(toolbox.typecheck(x11)))
    }
    println("========================================")
    locally {
      val x11: Tree = q"""1: String"""
      println("- quote x11:\n" + x11)

      val x12: Try[_] = Try(toolbox.compile(x11))
      println("- Try(toolbox.compile(x11)) x12:\n" + x12)

      println("- x12.failed.get:\n" + x12.failed.get)
      println("- x12.failed.get.getMessage:\n" + x12.failed.get.getMessage)
      println("- x12.failed.get.getCause:\n" + x12.failed.get.getCause)

      // shows "scala.Predef.println" (vs. just "println" from q"...")
      println("- Try(toolbox.typecheck(x11)):\n" + Try(toolbox.typecheck(x11)))
    }


  }

}
