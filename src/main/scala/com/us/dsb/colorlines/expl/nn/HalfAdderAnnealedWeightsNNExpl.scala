package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.types.ArrayTypes
import com.us.dsb.colorlines.expl.nn.types.ArrayTypes.{
  LayerActivations, LayerBiases, LayerParameters, LayerWeights}
import com.us.dsb.colorlines.expl.nn.types.ScalarTypes.{Activation, Bias, Weight, raw}
import com.us.dsb.colorlines.expl.nn.TypesToSort.{
  OneHiddenNeuralNetworkWeightsAndBiases, OneHiddenTopology}

import scala.util.Random

object HalfAdderAnnealedWeightsNNExpl extends App {

  case class ZeroesOneHiddenNeuralNetworkWeightsAndBiases(topology: OneHiddenTopology)
      extends OneHiddenNeuralNetworkWeightsAndBiases {
    import topology.{inputLayerSize, hiddenLayerSize, outputLayerSize}
    override val hiddenLayer: LayerParameters =
      LayerParameters(hiddenLayerSize,
                      LayerBiases.fill(hiddenLayerSize)(Bias(0)),
                      LayerWeights.fill(hiddenLayerSize, inputLayerSize)(Weight(0)),
                      topology.inputLayerSize)
    override val outputLayer: LayerParameters =
      LayerParameters(topology.outputLayerSize,
                      LayerBiases.fill(outputLayerSize)(Bias(0)),
                      LayerWeights.fill(outputLayerSize, hiddenLayerSize)(Weight(0)),
                      hiddenLayerSize)
  }

  private def randomSomething: Double = {
    if true then {
      val `rand_0_to_1`: Double = Random.nextFloat()
      //?????? parameterize
      val `rand_-1_to_1`: Double = (`rand_0_to_1` - 0.5) * 2
      val `rand_-x_to_x`: Double = `rand_-1_to_1` * 10 // 20 // 5 // 20 //????
      //println(s"randomSomething: ${`rand_-x_to_x`}")
      `rand_-x_to_x`
    }
    else {
      Random.nextGaussian() * 20
    }
  }
  private def randomWeightIncr: Weight = Weight(randomSomething  / 10) // ?? same for now
  private def randomBiasIncr:   Bias   = Bias(  randomSomething  / 10) // ?? same for now

  class DerivedRandomOneHiddenNeuralNetworkWeightsAndBiases(
      base: OneHiddenNeuralNetworkWeightsAndBiases) extends OneHiddenNeuralNetworkWeightsAndBiases {
    val reductionFactor = 10.0

    override val hiddenLayer: LayerParameters =
      LayerParameters(
        base.hiddenLayer.size,
        LayerBiases(base.hiddenLayer.biases.vector.map(b => Bias(b.raw + randomBiasIncr.raw))),
        LayerWeights(
          base.hiddenLayer.weights.matrix.map { weights =>
            weights.map(w => Weight(w.raw + randomWeightIncr.raw))
          }),
        base.hiddenLayer.inputSize
      )
    override val outputLayer: LayerParameters =
      LayerParameters(
        base.outputLayer.size,
        LayerBiases(base.outputLayer.biases.vector.map(b => Bias(b.raw + randomBiasIncr.raw))),
        LayerWeights(
          base.outputLayer.weights.matrix.map { weights =>
            weights.map(w => Weight(w.raw + randomWeightIncr.raw))
          }),
        base.outputLayer.inputSize
        )
  }


  /**
   * ... without explicit neuron objects (with arrays) ... or stored activations ...
   */
  abstract class OneHiddenTopologyNeuralNetwork(val topology: OneHiddenTopology) {
    def weightsAndBiases: OneHiddenNeuralNetworkWeightsAndBiases

    def computeOutputActivations(inputActivations: LayerActivations
                                ): LayerActivations = {
      val hiddenLayerActivations =
        ArrayTypes.computeLayerActivation(inputActivations,
                                          weightsAndBiases.hiddenLayer)
      val outputLayerActivations =
        ArrayTypes.computeLayerActivation(hiddenLayerActivations,
                                          weightsAndBiases.outputLayer)
      outputLayerActivations
    }
  }

  case class RandomlyWeightedOneHiddenTopologyNeuralNetwork(override val topology: OneHiddenTopology)
      extends OneHiddenTopologyNeuralNetwork(topology) {
    override val weightsAndBiases = ZeroesOneHiddenNeuralNetworkWeightsAndBiases(topology)
  }

  case class DerivedRandomlyWeightedOneHiddenTopologyNeuralNetwork(
                                                                         base: OneHiddenTopologyNeuralNetwork
                                                                     )
      extends OneHiddenTopologyNeuralNetwork(base.topology) {
    override val weightsAndBiases = DerivedRandomOneHiddenNeuralNetworkWeightsAndBiases(base.weightsAndBiases)
  }


  //?????? rename; longer; "evaluate" sounds like evaluating fitness, vs. evaluating
  //  next function; ** "execute"? "compute"? ~~"evaluate function"
  //?????? Maybe wrap Byte in Bit opaque type to limit to 0 and 1:
  //?????? Possibly wrap Double in something reflecting approximated "bitness":
  def eval(nw: OneHiddenTopologyNeuralNetwork,
           inputs: (Byte, Byte, Byte)
          ): (Double, Double) = {

    // ???? TODO:  Is there way to map tuple to collection?  Or have fixed length
    // on collection?  (Tuple inputs and outputs were for fixed lengths for half-adder.)
    val inputActivations = LayerActivations(IndexedSeq(Activation(inputs._1),
                                                       Activation(inputs._2),
                                                       Activation(inputs._3)))  //??????
    val outputActivations  = nw.computeOutputActivations(inputActivations)
    (outputActivations.vector(0).raw,
        outputActivations.vector(1).raw)
  }

  // ?? TODO: _Possibly_ wrap Double in some opaque fitness type:
  def computeFitness(nw: OneHiddenTopologyNeuralNetwork): Double =
    HalfAdderCommon.computeFitness((a1: Byte, a2: Byte, a3: Byte) => eval(nw, (a1, a2, a3)))


  def makeHalfAdderNetwork: OneHiddenTopologyNeuralNetwork =
    RandomlyWeightedOneHiddenTopologyNeuralNetwork(OneHiddenTopology(3, 4, 2))

  def makeDerivedHalfAdderNetwork(base: OneHiddenTopologyNeuralNetwork): OneHiddenTopologyNeuralNetwork =
    DerivedRandomlyWeightedOneHiddenTopologyNeuralNetwork(base)


  val startMs = System.currentTimeMillis()
  var curr = makeHalfAdderNetwork
  var currFitness = computeFitness(curr)
  val maxIterations = 100_000_000
  var iterations = 0
  while iterations < maxIterations do {
    iterations += 1
    if (1 == iterations % 1_000_000) {
      val pct = 100.0 * iterations / maxIterations
      println(f"@ $iterations (/$maxIterations, ${pct}%4.1f%%) currFitness = $currFitness%11.5g ...")
    }
    //??????val cand = makeHalfAdderNetwork
    val cand = makeDerivedHalfAdderNetwork(curr)
    val candFitness = computeFitness(cand)

    if candFitness > currFitness then {
      println(f"@ $iterations: base: $currFitness%11.5g -> cand: $candFitness%11.5g")
      HalfAdderCommon.cases.foreach { case ((a1, a2, a3), (c, s)) =>
        val nnOutput = eval(cand, (a1, a2, a3))
        if ! true then
          println(f"$a1 + $a2 + $a3 = $c $s: ${nnOutput._1}%11.5g, ${nnOutput._2}%11.5g"
                      + f";  ∆c = ${nnOutput._1 - c}%11.5g"
                      + f", ∆s = ${nnOutput._2 - s}%11.5g")
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
