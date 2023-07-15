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
      if true then {
        val `rand_0_to_1`: Double = Random.nextFloat()
        //?????? parameterize
        val `rand_-1_to_1`: Double = (`rand_0_to_1` - 0.5) * 2
        val `rand_-x_to_x`: Double = `rand_-1_to_1` * 20 //????
        `rand_-x_to_x`
      }
      else {
        Random.nextGaussian() * 20
      }
    }

    private def randomBias = randomWeight // ?? same for now

    private trait Neuron {
      def updateActivation(): Unit
      def activation: Double
    }

    private case class InputNeuron() extends Neuron {
      var activation: Double = uninitialized

      override def updateActivation(): Unit = ()  // no-op
    }

    /** Neuron including incoming connections. */
    private case class NoninputNeuron(bias: Double,
                                      inputs: (Neuron, Double)*) extends Neuron {
      var activation: Double = uninitialized

      override def updateActivation(): Unit = {

        // Optimization:  About 10-15% faster overall as of 2023-07-13:
        // val weightedInputs =
        //   inputs.map { (neuron, weight) => neuron.activation * weight }
        // val sum1 = weightedInputs.sum
        var sum2 = 0.0
        inputs.foreach { (neuron, weight) => sum2 += neuron.activation * weight }

        val act = standardLogisticFunction(bias + sum2)
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
                                         hidden1s.map(in => (in, randomWeight))*))

    def setInputActivations(activations: Double*): Unit = {
      assert(ins.size == activations.size)
      ins.zip(activations).foreach((in, act) => in.activation = act)
    }

    def updateActivation(): Unit = {
      ins.foreach(_.updateActivation())  //???? handled by setInputActivations
      hidden1s.foreach(_.updateActivation())
      outs.foreach(_.updateActivation())
    }

    def getOutputActivations: IndexedSeq[Double] = {
      outs.map(_.activation)
      // Attempted optimization:  No detected speedup:
      //scala.collection.immutable.ArraySeq.unsafeWrapArray(outs.map(_.activation))
    }

  }

  def eval(nw: RandomlyWeightedNeuralNetwork,
           inputs: (Byte, Byte, Byte)
          ): (Double, Double) = {
    nw.setInputActivations(inputs._1, inputs._2, inputs._3)
    nw.updateActivation()
    val outActs = nw.getOutputActivations
    (outActs(0), outActs(1))
  }

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
    val fitness =
      cases.map { case ((a1, a2, a3), (c, s)) =>
        val nnOutput = eval(nw, (a1, a2, a3))
        val cError = nnOutput._1 - c
        val sError = nnOutput._2 - s
        val error = cError * cError + sError * sError
        val caseFitness = -error
        caseFitness
      }.sum
    fitness
  }

  def makeHalfAdderNetwork: RandomlyWeightedNeuralNetwork =
    RandomlyWeightedNeuralNetwork(3, 4, 2)

  val startMs = System.currentTimeMillis()
  var curr = makeHalfAdderNetwork
  var currFitness = computeFitness(curr)
  val maxIterations = 10_000_000
  var iterations = 0
  while (iterations < maxIterations) do {
    iterations += 1
    if (1 == iterations % 1_000_000) {
      println(f"@ $iterations (/$maxIterations) currFitness = $currFitness%8.5f ...")
    }
    val cand = makeHalfAdderNetwork
    val candFitness = computeFitness(cand)

    if candFitness > currFitness then {
      println(f"@ $iterations: base: $currFitness%8.5f -> cand: $candFitness%8.5f")
      cases.foreach { case ((a1, a2, a3), (c, s)) =>
        val nnOutput = eval(cand, (a1, a2, a3))
        println(f"$a1 + $a2 + $a3 = $c $s: ${nnOutput._1}%8.5f, ${nnOutput._2}%8.5f"
                    + f";  ∆c = ${nnOutput._1 - c}%8.5f"
                    + f", ∆s = ${nnOutput._2 - s}%8.5f")
      }
      curr = cand
      currFitness = candFitness
    }
  }
  val endMs = System.currentTimeMillis()
  val durationMs = endMs - startMs
  println(s"for $iterations iterations, durationMs = $durationMs ms"
              + s" (${durationMs * 1.0 / iterations} ms/iteration)")
}
