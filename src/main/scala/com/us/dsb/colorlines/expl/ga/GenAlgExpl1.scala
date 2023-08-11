package com.us.dsb.colorlines.expl.ga

import scala.util.Random

object GenAlgExpl1 extends App {

  val bitWidth = 32
  val populationSize = 5
  val anyMutationProbability = 0.1
  val perBitMutationProbability = 1.0 / bitWidth

  trait Individual {
    def bits: IndexedSeq[Byte]  // only 0: Byte or 1: Byte

    override def toString: String = {
      bits.mkString("<", " ", ">").replaceAll("(\\d ){8}", "$0 ")
    }
  }

  /** Fitness function--integer value of bits. */
  def computeFitness(i: Individual): Int = {
    i.bits.foldLeft(0)((accum, bit) => accum * 2 + bit)
  }

  case class IndividualImpl(bits: IndexedSeq[Byte]) extends Individual

  // create initial generation of randomly created individuals (full-sized generation)?

  def createRandomIndividual(): Individual = {
    IndividualImpl((1 to bitWidth).map(_ => Random.nextInt(2).toByte))
  }


  var population = (1 to populationSize).map(_ => createRandomIndividual())
  var highestFitness = population.map(i => computeFitness(i)).max

  def doCross(ma: Individual, pa: Individual): Individual = {
    val crossoverOffset = Random.nextInt(1 + bitWidth)  // 0 to N
    println(s"2. crossoverOffset = $crossoverOffset")
    val crossed = ma.bits.take(crossoverOffset) ++ pa.bits.drop(crossoverOffset)
    IndividualImpl(crossed)
  }

  def doMaybeMutate(i: Individual): Individual = {
    if (Random.nextFloat() >= anyMutationProbability)
      println("3. mutations: none")
      i
    else {
      println("3. mutations: maybe")
      val newBits =
        i.bits.zipWithIndex.map { (orig, offset) =>
          if (Random.nextFloat() >= perBitMutationProbability) {
            orig
          }
          else {
            println(s"3.1 mutations: flip at $offset")
            (1 - orig).toByte
          }
        }
      IndividualImpl(newBits)
    }
  }

  def breed(ma: Individual, pa: Individual): Individual = {
    val crossed = doCross(ma, pa)
    val mutated = doMaybeMutate(crossed)
    mutated
  }

  var breedingCount = 0
  while (highestFitness < Int.MaxValue) {
    println(s"1. generation = ${population.mkString("\n  ", "\n  ", "")}")
    println(s"1. highestFitness = $highestFitness")
    //Thread.sleep(10)

    breedingCount += 1
    val maOffset = Random.nextInt(population.size)
    val paOffset = Random.nextInt(population.size)
    val ma = population(maOffset)
    val pa = population(paOffset)
    val child = breed(ma, pa)
    println(s"4. ma =    $ma")
    println(s"4. child = $child")
    println(s"4. pa =    $pa")

    val maFitness    = computeFitness(ma)
    val paFitness    = computeFitness(pa)
    val childFitness = computeFitness(child)
    println(s"5. maFitness    = ${maFitness}")
    println(s"5. childFitness = ${childFitness}")
    println(s"5. paFitness    = ${paFitness}")

    val newPopulation =
      if (childFitness <= maFitness && childFitness <= paFitness) {
        println("6. forget child")
        population
      }
      else {
        println(s"6. better child        = ${child}")
        println(s"6.        childFitness = ${childFitness}")
        highestFitness = childFitness  //???? clean re updating var
        if (maFitness < paFitness) {
          println("6. replace ma")
          population.updated(maOffset, child)
        }
        else {
          println("6. replace pa")
          population.updated(paOffset, child)
        }
      }
    population = newPopulation
  }
  println(s"9. breedingCount = $breedingCount")

}
