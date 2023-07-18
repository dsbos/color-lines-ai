package com.us.dsb.colorlines.expl.nn

import scala.annotation.targetName
import scala.compiletime.uninitialized
import scala.util.Random

object HalfAdderRandomWeightsNNExpl extends App {
  
  object Types {

    opaque type Bias       = Double
    opaque type Weight     = Double
    opaque type Activation = Double

    object Bias       { def apply(value: Double): Bias       = value }
    object Weight     { def apply(value: Double): Weight     = value }
    object Activation { def apply(value: Double): Activation = value }

    extension (raw: Bias)       @targetName("Bias_raw")       def raw: Double = raw
    extension (raw: Weight)     @targetName("Weight_raw")     def raw: Double = raw
    extension (raw: Activation) @targetName("Activation_raw") def raw: Double = raw
  }
  import Types.*

  case class OneHiddenTopology(inputLayerSize : Int,
                               hiddenLayerSize: Int,
                               outputLayerSize: Int)

  case class RandomOneHiddenNeuralNetworkWeightsAndBiases(topology: OneHiddenTopology) {
    private def randomWeight: Weight = {
      if true then {
        val `rand_0_to_1`: Double = Random.nextFloat()
        //?????? parameterize
        val `rand_-1_to_1`: Double = (`rand_0_to_1` - 0.5) * 2
        val `rand_-x_to_x`: Double = `rand_-1_to_1` * 20  //????
        Weight(`rand_-x_to_x`)
      }
      else {
        Weight(Random.nextGaussian() * 20)
      }
    }

    private def randomBias: Bias = Bias(randomWeight.raw)  // ?? same for now

    import topology.*
    //?????? wrap Array usage (opaque type? regular class?)
    val hiddenLayerBiases: Array[Types.Bias] = Array.fill(hiddenLayerSize)(randomBias)
    val hiddenLayerInputWeights: Array[Array[Weight]] =  // indexed by hidden, then input
      Array.fill(hiddenLayerSize)(Array.fill(inputLayerSize)(randomWeight))
    val outputLayerBiases: Array[Bias] = Array.fill(outputLayerSize)(randomBias)
    val outputLayerInputWeights: Array[Array[Weight]] =  // indexed by output, then hidden
      Array.fill(outputLayerSize)(Array.fill(hiddenLayerSize)(randomWeight))
    print("")
  }

  //?????? change from mutable and updated by eval. method to constructed by eval. method
  case class OneHiddenActivations(topology: OneHiddenTopology) {
    import topology.*
    val inputLayer: Array[Activation] = Array.fill(inputLayerSize)(Activation(0))
    val hiddenLayer: Array[Activation] = Array.fill(hiddenLayerSize)(Activation(0))
    val outputLayer: Array[Activation] = Array.fill(outputLayerSize)(Activation(0))
  }

  /**
   * ... without explicit neuron objects (with arrays) ... still mutable activation ...
   */
  case class RandomlyWeightedOneHiddenTopologyNeuralNetwork2(topology: OneHiddenTopology) {
    private val activations: OneHiddenActivations = OneHiddenActivations(topology)
    private val weightsAndBiases = RandomOneHiddenNeuralNetworkWeightsAndBiases(topology)


    //?????? REWORK activation evaluation to take passed-in input and return
    // outputs (not updating mutable...):

    def setInputActivations(activations: Activation*): Unit = {
      assert(topology.inputLayerSize == activations.size)

      activations.zipWithIndex.foreach { (activation, inputIndex) =>
        this.activations.inputLayer(inputIndex) = activation
      }
    }

    def updateActivation(): Unit = {

      def updateLayerActivation(prevLayerSize: Int,  //?????? use activations-array size(?)
                                prevLayerActs: Array[Activation],
                                thisLayerSize: Int,  //?????? use activations-array size(?)
                                thisLayerBiases: Array[Bias],
                                thisLayerWeights: Array[Array[Weight]],
                                thisLayerActivations: Array[Activation]) = {
        for (thisIdx <- 0 until thisLayerSize) {

          val productVector =
            for (prevIdx <- 0 until prevLayerSize) yield {
              prevLayerActs(prevIdx).raw * thisLayerWeights(thisIdx)(prevIdx).raw
            }
          val sum = productVector.sum

          // // Optimization:  Avoids creating collection of products:  About 10-15%
          // // faster overall as of 2023-07-13:
          // var sum2 = 0.0
          // inputs.foreach { (neuron, weight) => sum2 += neuron.activation * weight }

          val bias = thisLayerBiases(thisIdx)
          //???????? check: does bias go _after_ activation function?
          val act = ActivationFunctions.standardLogisticFunction(bias.raw + sum)
          thisLayerActivations(thisIdx) = Activation(act)
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

    def getOutputActivations: IndexedSeq[Activation] = {
      activations.outputLayer
      // Attempted optimization:  No detected speedup:
      //scala.collection.immutable.ArraySeq.unsafeWrapArray(outs.map(_.activation))
    }

  }

  //?????? rename; longer; "evaluate" sounds like evaluating fitness, vs. evaluating
  //  next function; ** "execute"? "compute"? ~~"evaluate function"
  //?????? Maybe wrap Byte in Bit opaque type to limit to 0 and 1:
  //?????? Possibly wrap Double in something reflecting approximated "bitness":
  def eval(nw: RandomlyWeightedOneHiddenTopologyNeuralNetwork2,
           inputs: (Byte, Byte, Byte)
          ): (Double, Double) = {
    //?????? rework to pass in input and receive outputs (not updating mutable arrays...)

    // ???? TODO:  Is there way to map tuple to collection?  Or have fixed length
    // on collection?  (Tuple inputs and outputs were for fixed lengths for half-adder.)
    nw.setInputActivations(Activation(inputs._1),
                           Activation(inputs._2),
                           Activation(inputs._3))

    nw.updateActivation()
    val outActs = nw.getOutputActivations
    (outActs(0).raw, outActs(1).raw)
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

  // ?? TODO: _Possibly_ wrap Double in some opaque fitness type:
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
  val maxIterations = 100_000_000
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
