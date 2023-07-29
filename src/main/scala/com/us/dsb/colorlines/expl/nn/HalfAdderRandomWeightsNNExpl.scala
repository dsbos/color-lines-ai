package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.types.ArrayTypes
  import com.us.dsb.colorlines.expl.nn.types.ArrayTypes.{
  LayerActivations, LayerBiases, LayerParameters, LayerWeights}
import com.us.dsb.colorlines.expl.nn.types.ScalarTypes.{Activation, Bias, Weight}
import com.us.dsb.colorlines.expl.nn.TypesToSort.{
  OneHiddenNeuralNetworkWeightsAndBiases, OneHiddenTopology}

import scala.util.Random

object HalfAdderRandomWeightsNNExpl extends App {
  
  case class RandomOneHiddenNeuralNetworkWeightsAndBiases(topology: OneHiddenTopology)
      extends OneHiddenNeuralNetworkWeightsAndBiases {
    private def randomSomething: Double = {
      if true then {
        val `rand_0_to_1`: Double = Random.nextFloat()
        //?????? parameterize
        val `rand_-1_to_1`: Double = (`rand_0_to_1` - 0.5) * 2
        val `rand_-x_to_x`: Double = `rand_-1_to_1` * 20  //????
        //println(s"randomSomething: ${`rand_-x_to_x`}")
        `rand_-x_to_x`
      }
      else {
        Random.nextGaussian() * 20
      }
    }

    private def randomWeight: Weight = Weight(randomSomething)  // ?? same for now
    private def randomBias: Bias = Bias(randomSomething)  // ?? same for now

    import topology.*
    override val hiddenLayer: LayerParameters =
      LayerParameters(topology.hiddenLayerSize,
                      LayerBiases.fill(hiddenLayerSize)(randomBias),
                      LayerWeights.fill(hiddenLayerSize, inputLayerSize)(randomWeight),
                      topology.inputLayerSize)
    override val outputLayer: LayerParameters =
      LayerParameters(topology.outputLayerSize,
                      LayerBiases.fill(outputLayerSize)(randomBias),
                      LayerWeights.fill(outputLayerSize, hiddenLayerSize)(randomWeight),
                      topology.hiddenLayerSize)
    print("")
  }

  private val activationFunction: Double => Activation =
    raw => Activation(ActivationFunctions.standardLogisticFunction(raw))  //????????

  /**
   * ... without explicit neuron objects (with arrays) ... or stored activations ...
   */
  case class RandomlyWeightedOneHiddenTopologyNeuralNetwork2(topology: OneHiddenTopology) {
    private val weightsAndBiases: OneHiddenNeuralNetworkWeightsAndBiases =
      RandomOneHiddenNeuralNetworkWeightsAndBiases(topology)

    def computeOutputActivations(inputActivations: LayerActivations
                                ): LayerActivations = {
      val hiddenLayerActivations =
        ArrayTypes.computeLayerActivation(inputActivations,
                                          weightsAndBiases.hiddenLayer,
                                          activationFunction)
      val outputLayerActivations =
        ArrayTypes.computeLayerActivation(hiddenLayerActivations,
                                          weightsAndBiases.outputLayer,
                                          activationFunction)
      outputLayerActivations
    }
  }

  //?????? rename; longer; "evaluate" sounds like evaluating fitness, vs. evaluating
  //  net function; "execute"? "compute output"? ~~"evaluate function"?  "evaluate
  //  network"  "calculate output"?  ("transform"?)
  //?????? Maybe wrap Byte in Bit opaque type to limit to 0 and 1:
  //?????? Possibly wrap Double in something reflecting approximated "bitness":
  def eval(nw: RandomlyWeightedOneHiddenTopologyNeuralNetwork2,
           inputs: (Byte, Byte, Byte)
          ): (Double, Double) = {

    // ???? TODO:  Is there way to map tuple to collection?  Or have fixed length on
    //   collection?  (Tuple inputs and outputs are for fixed lengths for half-adder.)
    // ???? TODO:  Should LayerActivations have var-args ~constructor?
    val inputActivations = LayerActivations(IndexedSeq(Activation(inputs._1),
                                                       Activation(inputs._2),
                                                       Activation(inputs._3)))
    val outputActivations  = nw.computeOutputActivations(inputActivations)
    (outputActivations.vector(0).raw,
        outputActivations.vector(1).raw)
  }

  // ?? TODO: _Possibly_ wrap Double in some opaque fitness type:
  //????????
  def computeFitness(nw: RandomlyWeightedOneHiddenTopologyNeuralNetwork2): Double =
    HalfAdderCommon.computeFitness((a1: Byte, a2: Byte, a3: Byte) => eval(nw, (a1, a2, a3)))

  def makeHalfAdderNetwork: RandomlyWeightedOneHiddenTopologyNeuralNetwork2 =
    RandomlyWeightedOneHiddenTopologyNeuralNetwork2(OneHiddenTopology(3, 4, 2))
  //????????

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
    val cand = makeHalfAdderNetwork  //????????
    val candFitness = computeFitness(cand)

    if candFitness > currFitness then {
      println(f"@ $iterations: base: $currFitness%8.5f -> cand: $candFitness%8.5f")
      HalfAdderCommon.cases.foreach { case ((a1, a2, a3), (c, s)) =>
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
