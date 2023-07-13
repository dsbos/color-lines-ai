package com.us.dsb.colorlines.expl.nn

import scala.compiletime.uninitialized
import scala.util.Random

object HalfAdderRandomWeightsNNExpl extends App {

  /**
   * @param x0
   *   x values of function's midpoint
   * @param L
   *   supremum of the function
   * @param k
   *   logistics grawth rate
   * @param x
   * @return
   */
  def logisticFunction(x0: Double, L: Double, k: Double, x: Double): Double = {
    1 / (1 + math.exp(-(x - x0)))
  }

  def standardLogisticFunction(x: Double): Double = {
    logisticFunction(x0 = 0, L = 1, k = 1, x)
  }


//  for (i <- 1 to 20) {
//    val xx1 = (Random.nextFloat() - 0.5) * 10
//    val x = xx1   //   * xx1 * math.signum(xx1)
//
//    val f = standardLogisticFunction(x)
//    //println(f"f(${x}%10.2f) = $f%10.5f")
//
//  }

  case class Network342() {
    private def randomWeight = {
      val `rand_0_to_1`: Double = Random.nextFloat()
      val `rand_-1_to_1`: Double = (`rand_0_to_1` - 0.5) * 2
      val `rand_-x_to_x`: Double = `rand_-1_to_1` * 20
      `rand_-x_to_x`
    }
    private def randomBias = randomWeight // ??? for now

    trait Neuron {
      def computeActivation(): Unit
      def activation: Double
    }

    case class InputNeuron() extends Neuron {
      var inputValue: Byte = 0  //?? 0 or 1
      var activation: Double = uninitialized

      def computeActivation(): Unit =
        activation = inputValue
    }

    case class NoninputNeuron(bias: Double, inputs: (Neuron, Double)*) extends Neuron {
      var activation: Double = uninitialized

      def computeActivation(): Unit = {
        val x = inputs.map { (neuron, weight) => neuron.activation * weight }
        val sum = x.sum
        val biased = bias + sum
        val act = standardLogisticFunction(sum)
        activation = act
      }
    }

    val in1: InputNeuron = InputNeuron()
    val in2: InputNeuron = InputNeuron()
    val in3: InputNeuron = InputNeuron()

    val hidden4: NoninputNeuron = NoninputNeuron(randomBias,
                                                 (in1, randomWeight),
                                                 (in2, randomWeight),
                                                 (in3, randomWeight))
    val hidden5: NoninputNeuron = NoninputNeuron(randomBias,
                                                 (in1, randomWeight),
                                                 (in2, randomWeight),
                                                 (in3, randomWeight))
    val hidden6: NoninputNeuron = NoninputNeuron(randomBias,
                                                 (in1, randomWeight),
                                                 (in2, randomWeight),
                                                 (in3, randomWeight))
    val hidden7: NoninputNeuron = NoninputNeuron(randomBias,
                                                 (in1, randomWeight),
                                                 (in2, randomWeight),
                                                 (in3, randomWeight))

    val out8: NoninputNeuron = NoninputNeuron(randomBias,
                                              (hidden4, randomWeight),
                                              (hidden5, randomWeight),
                                              (hidden6, randomWeight),
                                              (hidden7, randomWeight))
    val out9: NoninputNeuron = NoninputNeuron(randomBias,
                                              (hidden4, randomWeight),
                                              (hidden5, randomWeight),
                                              (hidden6, randomWeight),
                                              (hidden7, randomWeight))


    def eval(inputs: (Byte, Byte, Byte)): (Double, Double) = {
      in1.inputValue = inputs._1
      in2.inputValue = inputs._2
      in3.inputValue = inputs._3
      in1.computeActivation()
      in2.computeActivation()
      in3.computeActivation()
      hidden4.computeActivation()
      hidden5.computeActivation()
      hidden6.computeActivation()
      hidden7.computeActivation()
      out8.computeActivation()
      out9.computeActivation()
      (out8.activation, out9.activation)
    }
  }
  val x: Byte = 0

  val cases: List[((Byte, Byte, Byte), (Byte, Byte))] = {
    List( // 3 bits to add -> 2-bit sum (high bit, low bit)
          (0, 0, 0) -> (0, 0),
          (0, 0, 1) -> (0, 1),
          (0, 1, 0) -> (0, 1),
          (0, 1, 1) -> (1, 0),
          (1, 0, 0) -> (0, 1),
          (1, 0, 1) -> (1, 0),
          (1, 1, 0) -> (1, 0),
          (1, 1, 1) -> (1, 1),
          )
        .map { case ((a1, a2, a3), (c, s)) =>
          ((a1.toByte, a2.toByte, a3.toByte), (c.toByte, s.toByte))
        }
  }

  def computeFitness(nw: Network342): Double = {
    cases.map { case ((a1, a2, a3), (c, s)) =>
      val nnOutput = nw.eval(a1, a2, a3)
      val cError = nnOutput._1 - c
      val sError = nnOutput._2 - s
      val error = cError * cError + sError * sError
      val caseFitness = -error
      caseFitness
    }.sum
  }

  var prev = Network342()
  var iterations = 0
  while (iterations < 1_000_000_000) do {
    iterations += 1
    val cand = Network342()
    val prevFitness = computeFitness(prev)
    val candFitness = computeFitness(cand)

    //println(s"prev: = $prevFitness, cand: = $candFitness")
    if candFitness > prevFitness then {
      println(s"@ $iterations: prev: = $prevFitness -> cand: = $candFitness")
      cases.foreach { case ((a1, a2, a3), (c, s)) =>
        val nnOutput = prev.eval(a1, a2, a3)
        println(f"$a1 + $a2 + $a3 = $c $s: ${nnOutput._1}%7.3f, ${nnOutput._2}%7.3f")
      }
      prev = cand
    }

  }
}
