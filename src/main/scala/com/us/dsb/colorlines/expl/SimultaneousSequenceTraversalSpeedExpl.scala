package com.us.dsb.colorlines.expl

/**
 * Exploration of speed of traversing two sequences in parallel (as for vector
 * dot product (e.g., neural network activations times weights)).
 *
 * Typical result (2023-07-27):
 *   ...
 *   Pass 2:
 *   dur.  1 = 991 ms (no view,     zip, map w/two params,  .sum)
 *   dur.  2 = 987 ms (no view,     zip, map w/Tuple param, .sum)
 *   dur.  3 = 1781 ms (   view,     zip, map w/two params,  .sum)
 *   dur.  4 = 1296 ms (   view,     zip, map w/Tuple param, .sum)
 *   dur.  5 = 1793 ms (no view, lazyZip, map w/Tuple param, .sum)
 *   dur.  6 = 1354 ms (no view, lazyZip, map w/two params,  .sum)
 *   dur.  7 = 1340 ms (   view, lazyZip, map w/Tuple param, .sum)
 *   dur.  8 = 1061 ms (   view, lazyZip, map w/two params,  .sum)
 *   dur. *9 = 470 ms (indexed, var, for on .indices)
 *   dur. 10 = 574 ms (indexed, var, while on live acts.indices.end)
 *   dur.*11*= 427 ms (indexed, var, while on copied end)
 *   dur. 12 = 574 ms (indexed, var, while on copied end; via lambda)
 *   dur. 13 = 492 ms (indexed, var, foldLeft on .indices)
*/
object SimultaneousSequenceTraversalSpeedExpl extends App {
  val acts = IndexedSeq(1, 2, 3)
  val weights = IndexedSeq(10, 20, 30)
  val expectedSum = 140

  val iterations = 10_000_000
  for (pass <- 1 to 2) {
    println(s"Pass $pass:")

    {
      val start = System.currentTimeMillis()
      for (i <- 1 to iterations) {
        val sum = acts.zip(weights).map((a, w) => a * w).sum
        assert(sum == expectedSum)
      }
      println("dur.  1 = " + (System.currentTimeMillis() - start) + " ms (no view,     zip, map w/two params,  .sum)")
    }

    {
      val start = System.currentTimeMillis()
      for (i <- 1 to iterations) {
        val sum = acts.zip(weights).map(t => t._1 * t._2).sum
        assert(sum == expectedSum)
      }
      println("dur.  2 = " + (System.currentTimeMillis() - start) + " ms (no view,     zip, map w/Tuple param, .sum)")
    }

    {
      val start = System.currentTimeMillis()
      for (i <- 1 to iterations) {
        val sum = acts.view.zip(weights).map((a, w) => a * w).sum
        assert(sum == expectedSum)
      }
      println("dur.  3 = " + (System.currentTimeMillis() - start) + " ms (   view,     zip, map w/two params,  .sum)")
    }

    {
      val start = System.currentTimeMillis()
      for (i <- 1 to iterations) {
        val sum = acts.view.zip(weights).map(t => t._1 * t._2).sum
        assert(sum == expectedSum)
      }
      println("dur.  4 = " + (System.currentTimeMillis() - start) + " ms (   view,     zip, map w/Tuple param, .sum)")
    }

    {
      val start = System.currentTimeMillis()
      for (i <- 1 to iterations) {
        val sum = acts.lazyZip(weights).map(t => t._1 * t._2).sum
        assert(sum == expectedSum)
      }
      println("dur.  5 = " + (System.currentTimeMillis() - start) + " ms (no view, lazyZip, map w/Tuple param, .sum)")
    }

    {
      val start = System.currentTimeMillis()
      for (i <- 1 to iterations) {
        val sum = acts.lazyZip(weights).map((a, w) => a * w).sum
        assert(sum == expectedSum)
      }
      println("dur.  6 = " + (System.currentTimeMillis() - start) + " ms (no view, lazyZip, map w/two params,  .sum)")
    }

    {
      val start = System.currentTimeMillis()
      for (i <- 1 to iterations) {
        val sum = acts.view.lazyZip(weights).map(t => t._1 * t._2).sum
        assert(sum == expectedSum)
      }
      println("dur.  7 = " + (System.currentTimeMillis() - start) + " ms (   view, lazyZip, map w/Tuple param, .sum)")
    }

    {
      val start = System.currentTimeMillis()
      for (i <- 1 to iterations) {
        val sum = acts.view.lazyZip(weights).map((a, w) => a * w).sum
        assert(sum == expectedSum)
      }
      println("dur.  8 = " + (System.currentTimeMillis() - start) + " ms (   view, lazyZip, map w/two params,  .sum)")
    }

    {
      val start = System.currentTimeMillis()
      for (i <- 1 to iterations) {
        var sumAccum: Double = 0
        for (inputIdx <- acts.indices) {
          sumAccum += acts(inputIdx) * weights(inputIdx)
        }
        assert(sumAccum == expectedSum)
      }
      println("dur. *9 = " + (System.currentTimeMillis() - start) + " ms (indexed, var, for on .indices)")
    }

    {
      val start = System.currentTimeMillis()
      for (i <- 1 to iterations) {
        var sumAccum: Double = 0
        var inputIdx = acts.indices.start
        while (inputIdx < acts.indices.end) {
          sumAccum += acts(inputIdx) * weights(inputIdx)
          inputIdx += 1
        }
        assert(sumAccum == expectedSum)
      }
      println("dur. 10 = " + (System.currentTimeMillis() - start) + " ms (indexed, var, while on live acts.indices.end)")
    }

    // Fastest:
    {
      val start = System.currentTimeMillis()
      for (i <- 1 to iterations) {
        var sumAccum: Double = 0
        var inputIdx = acts.indices.start
        val end = acts.indices.end
        while (inputIdx < end) {
          sumAccum += acts(inputIdx) * weights(inputIdx)
          inputIdx += 1
        }
        assert(sumAccum == expectedSum)
      }
      println("dur.*11*= " + (System.currentTimeMillis() - start) + " ms (indexed, var, while on copied end)")
    }

    {
      val start = System.currentTimeMillis()
      for (i <- 1 to iterations) {
        val actsFn   : Int => Int = idx => acts(idx)
        val weightsFn: Int => Int = idx => weights(idx)

        var sumAccum: Double = 0
        var inputIdx = acts.indices.start
        val end = acts.indices.end
        while (inputIdx < end) {
          sumAccum += actsFn(inputIdx) * weightsFn(inputIdx)
          inputIdx += 1
        }
        assert(sumAccum == expectedSum)
      }
      println("dur. 12 = " + (System.currentTimeMillis() - start) + " ms (indexed, var, while on copied end; via lambda)")
    }

    {
      val start = System.currentTimeMillis()
      for (i <- 1 to iterations) {
        val sum = acts.indices.foldLeft(0.0) { (sumAccum, inputIdx) =>
          sumAccum + acts(inputIdx) * weights(inputIdx)
        }
        assert(sum == expectedSum)
      }
      println("dur. 13 = " + (System.currentTimeMillis() - start) + " ms (indexed, var, foldLeft on .indices)")
    }

    // No .zipped--deprecates
    // (Iterable?)
  }

}
