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
   *
   * @return
   */
  def logisticFunction(x0: Double, L: Double, k: Double, x: Double): Double = {
    1 / (1 + math.exp(-(x - x0)))
  }

  /** ... zero x0, L, and k */
  def standardLogisticFunction(x: Double): Double = {
    logisticFunction(x0 = 0, L = 1, k = 1, x)
  }

  case class OneHiddenTopology(inputLayerSize : Int,
                               hiddenLayerSize: Int,
                               outputLayerSize: Int)

  case class RandomOneHiddenNeuralNetworkWeightsAndBiases(topology: OneHiddenTopology) {
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

    import topology.*
    //?????? wrap Array usage (opaque type? regular class?)
    val hiddenLayerBiases: Array[Double] = Array.fill(hiddenLayerSize)(randomBias)
    val hiddenLayerInputWeights: Array[Array[Double]] = // indexed by hidden, then input
      Array.fill(hiddenLayerSize)(Array.fill(inputLayerSize)(randomWeight))
    val outputLayerBiases: Array[Double] = Array.fill(outputLayerSize)(randomBias)
    val outputLayerInputWeights: Array[Array[Double]] = // indexed by output, then hidden
      Array.fill(outputLayerSize)(Array.fill(hiddenLayerSize)(randomWeight))
    print("")
  }

  //?????? change from mutable and updated by eval. metjhod to constructed by eval. method
  case class OneHiddenActivations(topology: OneHiddenTopology) {
    import topology.*
    val inputLayer: Array[Double] = Array.fill(inputLayerSize)(0)
    val hiddenLayer: Array[Double] = Array.fill(hiddenLayerSize)(0)
    val outputLayer: Array[Double] = Array.fill(outputLayerSize)(0)
  }

  /**
   * ... without explicit neuron objects (with arrays) ... still mutable activation ...
   */
  case class RandomlyWeightedOneHiddenTopologyNeuralNetwork2(topology: OneHiddenTopology) {
    val activations: OneHiddenActivations = OneHiddenActivations(topology)
    val weightsAndBiases = RandomOneHiddenNeuralNetworkWeightsAndBiases(topology)


    //?????? REWORK activation evaluation to tale passed-in input and return
    // outputs (not updating mutable...):

    def setInputActivations(activations: Double*): Unit = {
      assert(topology.inputLayerSize == activations.size)

      activations.zipWithIndex.foreach { (activation, inputIndex) =>
        this.activations.inputLayer(inputIndex) = activation
      }
    }

    def updateActivation(): Unit = {

      def updateLayerActivation(prevLayerSize: Int,  //?????? use activations-array size(?)
                                prevLayerActs: Array[Double],
                                thisLayerSize: Int,  //?????? use activations-array size(?)
                                thisLayerBiases: Array[Double],
                                thisLayerWeights: Array[Array[Double]],
                                thisLayerActivations: Array[Double]) = {
        for (thisIdx <- 0 until thisLayerSize) {

          val productVector =
            for (prevIdx <- 0 until prevLayerSize) yield {
              prevLayerActs(prevIdx) * thisLayerWeights(thisIdx)(prevIdx)
            }
          val sum = productVector.sum

          // // Optimization:  Avoids creating collection of products:  About 10-15%
          // // faster overall as of 2023-07-13:
          // var sum2 = 0.0
          // inputs.foreach { (neuron, weight) => sum2 += neuron.activation * weight }

          val bias = thisLayerBiases(thisIdx)
          //???????? check: does bias go _after_ activation function?
          val act = standardLogisticFunction(bias + sum)
          thisLayerActivations(thisIdx) = act
        }
      }

      updateLayerActivation(topology.inputLayerSize,
                            activations.inputLayer,
                            topology.hiddenLayerSize,
                            weightsAndBiases.hiddenLayerBiases,
                            weightsAndBiases.hiddenLayerInputWeights,
                            activations.hiddenLayer)

      updateLayerActivation(topology.hiddenLayerSize,
                            activations.hiddenLayer,
                            topology.outputLayerSize,
                            weightsAndBiases.outputLayerBiases,
                            weightsAndBiases.outputLayerInputWeights,
                            activations.outputLayer)
    }

    def getOutputActivations: IndexedSeq[Double] = {
      activations.outputLayer
      // Attempted optimization:  No detected speedup:
      //scala.collection.immutable.ArraySeq.unsafeWrapArray(outs.map(_.activation))
    }

  }


  /**
   * ... with explicit neuron objects (with arrays) ... mutable activation ...
   */
  case class xxRandomlyWeightedOneHiddenTopologyNeuralNetwork1(topology: OneHiddenTopology,
                                                                  inSize: Int,
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

  //?????? rename; longer; "evaluate" sounds like evaluating fitness, vs. evaluating
  //  next function; ** "execute"? "compute"? ~~"evaluate function"
  def eval(nw: RandomlyWeightedOneHiddenTopologyNeuralNetwork2,
           inputs: (Byte, Byte, Byte)
          ): (Double, Double) = {
    //?????? rework to pass in input and receive outputs (not updating mutable...)
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

  def computeFitness(nw: RandomlyWeightedOneHiddenTopologyNeuralNetwork2): Double = {
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

  def makeHalfAdderNetwork: RandomlyWeightedOneHiddenTopologyNeuralNetwork2 =
    RandomlyWeightedOneHiddenTopologyNeuralNetwork2(OneHiddenTopology(3, 4, 2))

  val startMs = System.currentTimeMillis()
  var curr = makeHalfAdderNetwork
  var currFitness = computeFitness(curr)
  val maxIterations = 10_000_000
  var iterations = 0
  while iterations < maxIterations do {
    iterations += 1
    if (1 == iterations % 1_000_000) {
      val pct = 100.0 * iterations / maxIterations
      println(f"@ $iterations (/$maxIterations, ${pct}%4.1f%%) currFitness = $currFitness%8.5f ...")
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
