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


  /**
   * ... (Mutable activation.)
   */
  case class RandomlyWeightedNeuralNetwork(inSize: Int,
                                           hiddenSize: Int,
                                           outSize: Int) {
    private def randomWeight = {
      val `rand_0_to_1`: Double = Random.nextFloat()
      //?????? parameterize
      val `rand_-1_to_1`: Double = (`rand_0_to_1` - 0.5) * 2
      val `rand_-x_to_x`: Double = `rand_-1_to_1` * 20
      `rand_-x_to_x`
    }
    private def randomBias = randomWeight // ?? same for now

    trait Neuron {
      def computeActivation(): Unit
      def activation: Double
    }

    case class InputNeuron() extends Neuron {
      //?????? probably move domain-specific Byte/inputValue out from generic activation
      var inputValue: Byte = 0  //?? 0 or 1
      var activation: Double = uninitialized

      override def computeActivation(): Unit =
        activation = inputValue
    }

    /** Neuron including incoming connections. */
    case class NoninputNeuron(bias: Double, inputs: (Neuron, Double)*) extends Neuron {
      var activation: Double = uninitialized

      override def computeActivation(): Unit = {
        val weightedInputs = inputs.map { (neuron, weight) => neuron.activation * weight }
        val act = standardLogisticFunction(bias + weightedInputs.sum)
        activation = act
      }
    }

    //???? eventually parameterize number of hidden layers (and different sizes?)
    private val ins =
      Array.fill(inSize)(InputNeuron())
    private val hidden1s =
      Array.fill(hiddenSize)(NoninputNeuron(randomBias,
                                            ins.map(in => (in, randomWeight))*))
    private val outs =
      Array.fill(outSize)(NoninputNeuron(randomBias,
                                         hidden1s.map(in => (in, randomWeight)) *))

    //?????? probably move domain-specific Byte and Tuple2 out from generic NN operations
    def eval(inputs: (Byte, Byte, Byte)): (Double, Double) = {
      ins(0).inputValue = inputs._1
      ins(1).inputValue = inputs._2
      ins(2).inputValue = inputs._3
      ins.foreach(_.computeActivation())
      hidden1s.foreach(_.computeActivation())
      outs.foreach(_.computeActivation())
      (outs(0).activation, outs(1).activation)
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

  def computeFitness(nw: RandomlyWeightedNeuralNetwork): Double = {
    cases.map { case ((a1, a2, a3), (c, s)) =>
      val nnOutput = nw.eval(a1, a2, a3)
      val cError = nnOutput._1 - c
      val sError = nnOutput._2 - s
      val error = cError * cError + sError * sError
      val caseFitness = -error
      caseFitness
    }.sum
  }

  var base = RandomlyWeightedNeuralNetwork(3, 4, 3)
  var iterations = 0
  while (iterations < 1_000_000_000) do {
    iterations += 1
    val cand = RandomlyWeightedNeuralNetwork(3, 4, 2)
    val baseFitness = computeFitness(base)
    val candFitness = computeFitness(cand)

    //println(s"prev: = $prevFitness, cand: = $candFitness")
    if candFitness > baseFitness then {
      println(s"@ $iterations: base: = $baseFitness -> cand: = $candFitness")
      cases.foreach { case ((a1, a2, a3), (c, s)) =>
        val nnOutput = base.eval(a1, a2, a3)
        println(f"$a1 + $a2 + $a3 = $c $s: ${nnOutput._1}%7.3f, ${nnOutput._2}%7.3f")
      }
      base = cand
    }

  }
}
