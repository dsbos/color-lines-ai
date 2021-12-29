package com.us.dsb.explore.algs.eightqueens

object EightQueens {

  def solve1(): Unit = {
    for (q1Idx <- 0 to 63) {
      println("1.1")
      for (q2Idx <- 0 to 63) {
        println("1.2")
        // ...
      }
    }
  }

  def solve2(): Unit = {
    for (q1x <- 1 to 8) {
      println("2.1")
      for (q2x <- 1 to 8) {
        println("2.2")
        // ...
      }
    }
  }

  def solve3(order: Int = 8): Unit = {
    var solutionCount: Int = 0
    var callCount: Int = 0

    def solve(prevXs: Seq[Int]): Unit = {
      callCount += 1
      (prevXs.length : @unchecked) match {
        case `order` =>
          solutionCount += 1
          //println("Solution (x,y): " + prevXs.zipWithIndex.map({case (x, yoff) => (x, 1 + yoff)}))

        case yOff if 0 <= yOff && yOff < order =>
          val curY = yOff + 1
          for (curX <- 1 to order) {
            val okaySoFar =
              prevXs.zipWithIndex.forall({case (x, yoff) =>
                val y = 1 + yoff
                x != curX &&                 // different file
                y != curY &&                 // different rank
                                             // (redundant given implicit y indexing)
                (x + y) != (curX + curY) &&  // different x + y = C diagonal
                (x - y) != (curX - curY)     // different x - y = C diagonal
              })

            if (okaySoFar) {
              solve(prevXs :+ curX)
            }
          }
      }
    }

    val startNs = System.nanoTime()
    solve(Seq())
    val durationNs = System.nanoTime() - startNs
    println(f"3.9: order = $order%2d, solutionCount = $solutionCount%6d, callCount = $callCount%8d, \u00b5s = ${durationNs/1000}%7d")
  }

  def solve4(order: Int = 8): Unit = {
    var solutionCount: Int = 0
    var callCount: Int = 0
    case class ThreatenedLines(x: Int, y: Int, xPlusY: Int, xMinusY: Int)

    def solve(prevXs: Seq[Int], prevThreatened: Seq[ThreatenedLines]): Unit = {
      callCount += 1
      (prevXs.length : @unchecked) match {
        case `order` =>
          solutionCount += 1
          //println("Solution (x,y): " + prevXs.zipWithIndex.map({case (x, yoff) => (x, 1 + yoff)}))

        case yOff if 0 <= yOff && yOff < order =>
          val curY = yOff + 1
          for (curX <- 1 to order) {
            val okaySoFar =
              prevThreatened.forall(t => {
                t.x != curX &&                 // different file
                t.y != curY &&                 // different rank (redundant
                                               //   given implicit y indexing)
                t.xPlusY  != (curX + curY) &&  // different x + y = C diagonal
                t.xMinusY != (curX - curY)     // different x - y = C diagonal
              })

            if (okaySoFar) {
              solve(prevXs :+ curX, prevThreatened :+ ThreatenedLines(curX, curY, curX + curY, curX - curY))
            }
          }
      }
    }

    val startNs = System.nanoTime()
    solve(Seq(), Seq())
    val durationNs = System.nanoTime() - startNs
    println(f"4.9: order = $order%2d, solutionCount = $solutionCount%6d, callCount = $callCount%8d, \u00b5s = ${durationNs/1000}%7d")
  }

  def solve5(order: Int = 8): Unit = {
    import scala.collection.mutable.Seq
    var solutionCount: Int = 0
    var callCount: Int = 0
    case class ThreatenedLines(var x: Int, var y: Int, var xPlusY: Int, var xMinusY: Int)

    def solve(curDepth: Int, xs: Seq[Int], threatened: Seq[ThreatenedLines]): Unit = {
      callCount += 1
      (curDepth : @unchecked) match {
        case `order` =>
          solutionCount += 1
          //println("Solution (x,y): " + prevXs.zipWithIndex.map({case (x, yoff) => (x, 1 + yoff)}))

        case yOff if 0 <= yOff && yOff < order =>
          val curY = yOff + 1
          for (curX <- 1 to order) {
            val okaySoFar =
              // Note: The .take(...) seems to slow solve5 down (2x w.r.t. solve4, 3x w.r.t. solve6)
              threatened.take(curDepth).forall(t => {
                t.x != curX &&                 // different file
                t.y != curY &&                 // different rank (redundant
                                               //   given implicit y indexing)
                t.xPlusY  != (curX + curY) &&  // different x + y = C diagonal
                t.xMinusY != (curX - curY)     // different x - y = C diagonal
              })

            if (okaySoFar) {
              xs(curDepth) = curX
              val t = threatened(curDepth)
              t.x = curX
              t.y = curY
              t.xPlusY = curX + curY
              t.xMinusY = curX - curY
              solve(curDepth + 1, xs, threatened)
              /*
              t.x = Int.MinValue
              t.y = Int.MinValue
              t.xPlusY = Int.MinValue
              t.xMinusY = Int.MinValue
              */
            }
          }
      }
    }

    val startNs = System.nanoTime()
    solve(0,
          Seq.fill(order)(0),
          Seq.fill(order)(ThreatenedLines(Int.MinValue, Int.MinValue, Int.MinValue, Int.MinValue)))
    val durationNs = System.nanoTime() - startNs
    println(f"5.9: order = $order%2d, solutionCount = $solutionCount%6d, callCount = $callCount%8d, \u00b5s = ${durationNs/1000}%7d")
  }

  def solve6(order: Int = 8): Unit = {
    import scala.collection.mutable.Seq
    var solutionCount: Int = 0
    var callCount: Int = 0
    case class ThreatenedLines(var x: Int, var y: Int, var xPlusY: Int, var xMinusY: Int)

    def solve(curDepth: Int, xs: Seq[Int], threatened: Seq[ThreatenedLines]): Unit = {
      callCount += 1
      (curDepth : @unchecked) match {
        case `order` =>
          solutionCount += 1
          //println("Solution (x,y): " + prevXs.zipWithIndex.map({case (x, yoff) => (x, 1 + yoff)}))

        case yOff if 0 <= yOff && yOff < order =>
          val curY = yOff + 1
          for (curX <- 1 to order) {
            val okaySoFar =
              threatened.forall(t => {
                t.x != curX &&                 // different file
                t.y != curY &&                 // different rank (redundant
                                               //   given implicit y indexing)
                t.xPlusY  != (curX + curY) &&  // different x + y = C diagonal
                t.xMinusY != (curX - curY)     // different x - y = C diagonal
              })

            if (okaySoFar) {
              xs(curDepth) = curX
              val t = threatened(curDepth)
              t.x = curX
              t.y = curY
              t.xPlusY = curX + curY
              t.xMinusY = curX - curY
              solve(curDepth + 1, xs, threatened)
              t.x = Int.MinValue
              t.y = Int.MinValue
              t.xPlusY = Int.MinValue
              t.xMinusY = Int.MinValue
            }
          }
      }
    }

    val startNs = System.nanoTime()
    solve(0,
          Seq.fill(order)(0),
          Seq.fill(order)(ThreatenedLines(Int.MinValue, Int.MinValue, Int.MinValue, Int.MinValue)))
    val durationNs = System.nanoTime() - startNs
    println(f"6.9: order = $order%2d, solutionCount = $solutionCount%6d, callCount = $callCount%8d, \u00b5s = ${durationNs/1000}%7d")
  }

  def solve7(order: Int = 8): Unit = {
    import scala.collection.mutable.Seq
    var solutionCount: Int = 0
    var callCount: Int = 0
    case class ThreatenedLines(var x: Int, var y: Int, var xPlusY: Int, var xMinusY: Int)

    def solve(curDepth: Int, xs: Seq[Int], threatened: Seq[ThreatenedLines]): Unit = {
      callCount += 1
      (curDepth : @unchecked) match {
        case `order` =>
          solutionCount += 1
          //println("Solution (x,y): " + prevXs.zipWithIndex.map({case (x, yoff) => (x, 1 + yoff)}))

        case yOff if 0 <= yOff && yOff < order =>
          val curY = yOff + 1
          for (curX <- 1 to order) {
            val okaySoFar = {
              var okayImpl = true
              var idx = 0
              while (okayImpl && idx < curDepth) {
                val t = threatened(idx)
                if (! (
                    t.x != curX &&  // different file
                    t.y != curY &&  // different rank (redundant
                                    //   given implicit y indexing)
                    t.xPlusY != (curX + curY) &&  // different x + y = C diagonal
                    t.xMinusY != (curX - curY)    // different x - y = C diagonal
                    )) {
                  okayImpl = false
                }
                idx += 1
              }
              okayImpl
            }

            if (okaySoFar) {
              xs(curDepth) = curX
              val t = threatened(curDepth)
              t.x = curX
              t.y = curY
              t.xPlusY = curX + curY
              t.xMinusY = curX - curY
              solve(curDepth + 1, xs, threatened)
            }
          }
      }
    }

    val startNs = System.nanoTime()
    solve(0,
          Seq.fill(order)(0),
          Seq.fill(order)(ThreatenedLines(Int.MinValue, Int.MinValue, Int.MinValue, Int.MinValue)))
    val durationNs = System.nanoTime() - startNs
    println(f"7.9: order = $order%2d, solutionCount = $solutionCount%6d, callCount = $callCount%8d, \u00b5s = ${durationNs/1000}%7d")
  }

  def solve8(order: Int = 8): Unit = {
    import scala.collection.mutable.Seq
    var solutionCount: Int = 0
    var callCount: Int = 0

    // WHy doesn't "final val" inside ThreatenedLines make visible in this()'s call to this(...)?
    val FILES = order
    val RANKS = order
    val PLUS_DIAGS = order * 2 - 1
    val MINUS_DIAGS = order * 2 - 1
    val FILES_OFFSET = 1
    val RANKS_OFFSET = 1
    val PLUS_DIAGS_OFFSET = 2
    val MINUS_DIAGS_OFFSET = -(order - 1)

    class ThreatenedLines private(var xs: Array[Boolean],
                                  var ys: Array[Boolean],
                                  var xPlusYs: Array[Boolean],
                                  var xMinusYs: Array[Boolean]) {

      def this() = this(Array.fill(FILES)(false),
                        Array.fill(RANKS)(false),
                        Array.fill(PLUS_DIAGS)(false),
                        Array.fill(MINUS_DIAGS)(false))
    }

    def solve(curDepth: Int, xs: Seq[Int], threatened: Seq[ThreatenedLines]): Unit = {
      callCount += 1
      (curDepth : @unchecked) match {
        case `order` =>
          solutionCount += 1
          //println("Solution (x,y): " + prevXs.zipWithIndex.map({case (x, yoff) => (x, 1 + yoff)}))

        case yOff if 0 <= yOff && yOff < order =>
          val curY = yOff + 1
          for (curX <- 1 to order) {
            val okaySoFar = {
              var okayImpl = true
              var idx = 0
              while (okayImpl && idx < curDepth) {
                val t = threatened(idx)
                if (
                    t.xs(curX - FILES_OFFSET) ||  // file threatened
                    t.ys(curY - RANKS_OFFSET) ||  // rank threatened (redundant
                                      //   given implicit y indexing)
                    t.xPlusYs(curX + curY - PLUS_DIAGS_OFFSET) ||  // x + y = C diagonal threatened
                    t.xMinusYs(curX - curY - MINUS_DIAGS_OFFSET) // x - y = C diagonal threatened
                    ) {
                  okayImpl = false
                }
                idx += 1
              }
              okayImpl
            }

            if (okaySoFar) {
              xs(curDepth) = curX
              val t = threatened(curDepth)
              if (curDepth > 0) {
                val prevT = threatened(curDepth - 1)
                t.xs = prevT.xs  //????? doesn't deep copy
                t.ys = prevT.ys
                t.xPlusYs = prevT.xPlusYs
                t.xMinusYs = prevT.xMinusYs
              }

              t.xs(curX - FILES_OFFSET) = true
              t.ys(curY - RANKS_OFFSET) = true
              t.xPlusYs(curX + curY - PLUS_DIAGS_OFFSET) = true
              t.xMinusYs(curX - curY - MINUS_DIAGS_OFFSET) = true

              solve(curDepth + 1, xs, threatened)

              t.xs(curX - FILES_OFFSET) = false
              t.ys(curY - RANKS_OFFSET) = false
              t.xPlusYs(curX + curY - PLUS_DIAGS_OFFSET) = false
              t.xMinusYs(curX - curY - MINUS_DIAGS_OFFSET) = false
            }
          }
      }
    }

    val startNs = System.nanoTime()
    solve(0,
          Seq.fill(order)(0),
          Seq.fill(order)(new ThreatenedLines()))
    val durationNs = System.nanoTime() - startNs
    println(f"7.9: order = $order%2d, solutionCount = $solutionCount%6d, callCount = $callCount%8d, \u00b5s = ${durationNs/1000}%7d")
  }


  def main(args: Array[String]): Unit = {

    //solve1()
    //solve2()
    (1 to 3).foreach(_ => {
      println()
      val from = 8
      val to = from
      (from to to).foreach(order => solve3(order))
      (from to to).foreach(order => solve4(order))
      (from to to).foreach(order => solve5(order))
      (from to to).foreach(order => solve6(order))
      (from to to).foreach(order => solve7(order))
      (from to to).foreach(order => solve8(order))
     })
  }




}
