package com.us.dsb.explore.algs

/**
 * Solutions for BAH interview question, "find largest palindromic product
 * in ~cross product of range 1L to 999 timse range 1L to 999."
 */
object Palindromic1To999ProductsExpl {

  val MAX_FACTOR = 999
  println(s"MAX_FACTOR = $MAX_FACTOR")

  def isPalindromic(i: Int): Boolean = {
    val str = i.toString
    str == str.reverse
  }


  def doIt0(): (Int, Int) = {
    var iterations = 0

    val range = 1 to MAX_FACTOR
    val ijp =
      range.flatMap(i => range.map(j => (i, j, i * j)))
          .filter(ijp => {
            iterations += 1; isPalindromic(ijp._3)
          })
          .maxBy(ijp => ijp._3)
    val result = (ijp._1, ijp._2)
    println(s"doIt0: ~iterations: $iterations,  updates: ---, result = $result")
    result
  }

  def doIt1(): (Int, Int) = {
    var iterations = 0
    var updates = 0
    var maxProduct: Int = -1
    var maxProductI: Int = -1
    var maxProductJ: Int = -1

    for (i <- 1 to MAX_FACTOR; j <- 1 to MAX_FACTOR) {
      iterations += 1
      val product = i * j
      if (isPalindromic(product)) {
        if (product > maxProduct) {
          updates += 1
          maxProduct = product
          maxProductI = i
          maxProductJ = j
        }
      }
    }
    val result = (maxProductI, maxProductJ)
    println(s"doIt1: ~iterations: $iterations,  updates: $updates, result = $result")
    result
  }


  def doIt2(): Option[(Int, Int)] = {
    var iterations = 0
    var updates = 0
    var maxProduct: Option[Int] = None
    var maxProductI: Option[Int] = None
    var maxProductJ: Option[Int] = None

    for (i <- 1 to MAX_FACTOR; j <- 1 to MAX_FACTOR) {
      iterations += 1
      val product = i * j
      if (isPalindromic(product)) {
        if (maxProduct.fold(true)(product > _)) {
          updates += 1
          maxProduct = Some(product)
          maxProductI = Some(i)
          maxProductJ = Some(j)
        }
      }
    }
    val result = Some((maxProductI.get, maxProductJ.get))
    println(s"doIt2: ~iterations: $iterations,  updates: $updates, result = $result")
    result
  }

  def doIt3(): Option[(Int, Int)] = {
    var iterations = 0
    var updates = 0
    var maxProduct: Option[Int] = None
    var maxProductI: Option[Int] = None
    var maxProductJ: Option[Int] = None

    for (i <- 1 to MAX_FACTOR; j <- i to MAX_FACTOR) {
      iterations += 1
      val product = i * j
      if (isPalindromic(product)) {
        if (maxProduct.fold(true)(product > _)) {
          updates += 1
          maxProduct = Some(product)
          maxProductI = Some(i)
          maxProductJ = Some(j)
        }
      }
    }
    val result = Some((maxProductI.get, maxProductJ.get))
    println(s"doIt3: ~iterations: $iterations,  updates: $updates, result = $result")
    result
  }


  def doIt4(): Option[(Int, Int)] = {
    var iterations = 0
    var updates = 0
    var maxProduct: Option[Int] = None
    var maxProductI: Option[Int] = None
    var maxProductJ: Option[Int] = None

    for (i <- 1 to MAX_FACTOR
         if maxProduct.fold(true)(_ < i * MAX_FACTOR);
         j <- i to MAX_FACTOR) {
      iterations += 1
      val product = i * j
      if (isPalindromic(product)) {
        if (maxProduct.fold(true)(product > _)) {
          updates += 1
          maxProduct = Some(product)
          maxProductI = Some(i)
          maxProductJ = Some(j)
        }
      }
    }
    val result = Some((maxProductI.get, maxProductJ.get))
    println(s"doIt4: ~iterations: $iterations,  updates: $updates, result = $result")
    result
  }

  def doIt5(): Option[(Int, Int)] = {
    var iterations = 0
    var updates = 0
    var maxProduct: Option[Int] = None
    var maxProductI: Option[Int] = None
    var maxProductJ: Option[Int] = None

    for (i <- (1 to MAX_FACTOR).reverse
         if maxProduct.fold(true)(_ < i * MAX_FACTOR);
         j <- (i to MAX_FACTOR).reverse) {
      iterations += 1
      val product = i * j
      if (isPalindromic(product)) {
        if (maxProduct.fold(true)(product > _)) {
          updates += 1
          maxProduct = Some(product)
          maxProductI = Some(i)
          maxProductJ = Some(j)
          println(s"     - i = $i, j = $j, maxProduct := $maxProduct")
        }
      }
    }
    val result = Some((maxProductI.get, maxProductJ.get))
    println(s"doIt5: ~iterations: $iterations,  updates: $updates, result = $result")
    result
  }

  def doIt6(): Option[(Int, Int)] = {
    var iterations = 0
    var updates = 0
    var maxProduct: Option[Int] = None
    var maxProductI: Option[Int] = None
    var maxProductJ: Option[Int] = None

    // ???? only half of diagonals:
    for (diagNum <- 1 to MAX_FACTOR;
         nameThis2 = diagNum - 1;
         i <- (MAX_FACTOR - nameThis2) to MAX_FACTOR;
         j = MAX_FACTOR - nameThis2 + (MAX_FACTOR - i)) {
      //println(s"nameThis = $nameThis, nameThis2 = $nameThis2, i = $i, j = $j")
      iterations += 1
      val product = i * j
      if (isPalindromic(product)) {
        if (maxProduct.fold(true)(product > _)) {
          updates += 1
          maxProduct = Some(product)
          maxProductI = Some(i)
          maxProductJ = Some(j)
        }
      }
    }
    val result = Some((maxProductI.get, maxProductJ.get))
    println(s"doIt6: ~iterations: $iterations,  updates: $updates, result = $result")
    result
  }

  def doIt7(): Option[(Int, Int)] = {
    var iterations = 0
    var updates = 0
    var highestDiagNum = -1
    var maxProduct: Option[Int] = None
    var maxProductI: Option[Int] = None
    var maxProductJ: Option[Int] = None

    //println(s"- MAX_FACTOR = $MAX_FACTOR")
    // ???? only half of diagonals:

    for (diagNum <- 1 to MAX_FACTOR) {
      //println(s" - diagNum := $diagNum")
      val diagLength = diagNum

      for (dummy <- 1 to 1;
           diagOffset = diagNum - 1; //if {println(s"  - diagOffset := $diagOffset"); true};

           if maxProduct.fold(true)(max => {
             val temp1a = (MAX_FACTOR + MAX_FACTOR - diagOffset) / 2;
             val temp2a = (MAX_FACTOR + MAX_FACTOR - diagOffset + 1) / 2;
             //if {println(s"    - temp1a := $temp1a"); true};
             //if {println(s"    - temp2a := $temp2a"); true};
             //if {println(s"    - temp1a * temp2a := ${temp1a * temp2a}"); true};
             temp1a * temp2a > max
           });

           iRange = (MAX_FACTOR - diagOffset) to MAX_FACTOR; //if {println(s"  - iRange := $iRange"); true};
           i <- iRange;
           j = MAX_FACTOR - diagOffset + (MAX_FACTOR - i)
           ) {
        iterations += 1
        val product = i * j
        //println(s"     - diagNum = $diagNum, i = $i, j = $j, product = $product")
        highestDiagNum = highestDiagNum max diagNum
        if (isPalindromic(product)) {
          if (maxProduct.fold(true)(product > _)) {
            updates += 1
            maxProduct = Some(product)
            maxProductI = Some(i)
            maxProductJ = Some(j)
            println(s"     - diagNum = $diagNum, i = $i, j = $j, maxProduct := $maxProduct")
          }
        }
      }
    }
    val result = Some((maxProductI.get, maxProductJ.get))
    println(s" - highestDiagNum = $highestDiagNum")
    println(s"doIt7: ~iterations: $iterations,  updates: $updates, result = $result")
    result
  }

  def main(args: Array[String]): Unit = {
    doIt7()
    doIt5()
    doIt6()
    doIt4()
    doIt3()
    doIt2()
    doIt1()
    doIt0()
  }

}
