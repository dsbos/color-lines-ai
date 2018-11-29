package com.us.dsb.explore.movethis

object Scratch extends App {


  locally {
    def permute0[A](values: List[A]): Unit = {
      def permuteSub(prefix: List[A], toPermute: List[A]): Unit = {

        toPermute match {
          case Nil =>
            println("- Nil, prefix = " + prefix)
          case list =>
            for (subHead <- list) {
              val rest = list.filterNot(_ == subHead)
              permuteSub(prefix :+ subHead, rest)
            }
        }
      }
      permuteSub(Nil, values)
    }

    //permute0(Nil)
    //permute0(List(1, 2, 3))
  }
  println()


  // make little class to represent delivery (takeoff time, delivery time (incl. non), ~score, return time)?

  {
    def permuteWithLimit0(limit: Int, values: List[Int]): Unit = {

      def permuteSub(prefix: List[Int], prefixTotal: Int, toPermute: List[Int]): Unit = {
        val indentation = prefix.map(_ => "  ").mkString
        toPermute match {
          case Nil =>
            println(s"$indentation- Nil, prefixTotal = $prefixTotal, prefix = $prefix")
          case list =>
            var crudeSomethingPruner = false
            for (subHead <- list) {
              if (crudeSomethingPruner) {
                print("")
              }
              else {
                println(s"$indentation- trying " + subHead)
                val rest = list.filterNot(_ == subHead)
                val newTotal = prefixTotal + subHead
                val newPrefix = prefix :+ subHead

                if (newTotal > limit) {
                  println(s"$indentation- would go over limit ($limit): subHead = $subHead, newTotal = $newTotal, prefix = $prefix")
                  crudeSomethingPruner = true
                }
                else {
                  permuteSub(newPrefix, newTotal, rest)
                }
              }
            }
        }
      }
      permuteSub(Nil, 0, values.sorted)
    }

    //permuteWithLimit0(99, Nil)
    //permuteWithLimit0(99, List(1, 2, 3))
    permuteWithLimit0(6, List(1, 2, 3, 4))
  }




}
