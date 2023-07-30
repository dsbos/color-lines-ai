package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn2.ActivationComputation.{
  ActivationFunction, computeNetworkOutputActivation}
import com.us.dsb.colorlines.expl.nn2.ActivationFunctions
import com.us.dsb.colorlines.expl.nn2.types.LowlevelTypes.{
  Activation, Bias, LayerActivations, Weight, raw}
import com.us.dsb.colorlines.expl.nn2.types.NeuralNetworkSimpleImpl.{
  Layer, Network, Neuron}
import com.us.dsb.colorlines.expl.nn2.types.NeuralNetworkReadView.NetworkConfig

import scala.util.Random

object HalfAdderAnnealedWeightsNNExpl extends App {

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
  private def randomWeightIncr: Weight = Weight(randomSomething / 10)  // ?? same for now
  private def randomBiasIncr  : Bias   = Bias(randomSomething   / 10)  // ?? same for now

  /** ... zero biases and unity weights
   * @param inputSize
   * @param layerSizes
   *   ...; (non-input layers)
   * @return
   */
  // ????? TODO:  Probably add random-bias/-weight function parameters
  def makeBaseNetwork(inputSize: Int, layerSizes: Int*): NetworkConfig = {
    def unityWeights(count: Int): Seq[Weight] = Vector.fill(count)(Weight(1))
    def baseNeurons(neuronCount: Int, inputSize: Int): Seq[Neuron] =
      Vector.fill(neuronCount)(
        Neuron(inputSize, Bias(0), unityWeights(inputSize))
        )

    // Pair each non-input layer's size with predecessor's size:
    val layerAndInputSizePairs = layerSizes.zip(inputSize +: layerSizes.dropRight(1))
    val layers =
      layerAndInputSizePairs.map { (layerSize, inputSize) =>
        Layer(inputSize, baseNeurons(layerSize, inputSize))
      }
    Network(inputSize, layers)
  }

  // ?????? TODO:  Add temperature parameter; other?
  def randomlyDeriveNetwork(nw: NetworkConfig): NetworkConfig = {
    Network(nw.inputCount,
            nw.layers.map { layer =>
              Layer(layer.inputCount,
                    layer.neurons.map { neuron =>
                      Neuron(neuron.inputCount,
                             Bias(neuron.bias.raw + randomBiasIncr.raw),
                             neuron.weights.map { weight =>
                                Weight(weight.raw + randomWeightIncr.raw)
                             })
                    })
            })
  }

  private val activationFunction: ActivationFunction =
    raw => Activation(ActivationFunctions.standardLogisticFunction(raw))  //??????

  //?????? rename; longer; "evaluate" sounds like evaluating fitness, vs. evaluating
  //  next function; ** "execute"? "compute"? ~~"evaluate function"
  //?????? Maybe wrap Byte in Bit opaque type to limit to 0 and 1:
  //?????? Possibly wrap Double in something reflecting approximated "bitness":
  def eval(nw: NetworkConfig,
           inputs: (Byte, Byte, Byte)
          ): (Double, Double) = {

    // ???? TODO:  Is there way to map tuple to collection?  Or have fixed length
    // on collection?  (Tuple inputs and outputs were for fixed lengths for half-adder.)
    // ???? TODO:  Should LayerActivations have var-args ~constructor?
    val inputActivations = LayerActivations(IndexedSeq(Activation(inputs._1),
                                                       Activation(inputs._2),
                                                       Activation(inputs._3)))  //??????
    val outputActivations  =
      computeNetworkOutputActivation(nw, activationFunction, inputActivations)
    (outputActivations.vector(0).raw,
        outputActivations.vector(1).raw)
  }

  // ?? TODO: _Possibly_ wrap Double in some opaque fitness type:
  def computeFitness(nw: NetworkConfig): Double =
    HalfAdderCommon.computeFitness((a1: Byte, a2: Byte, a3: Byte) => eval(nw, (a1, a2, a3)))

  def makeHalfAdderNetwork: NetworkConfig = makeBaseNetwork(3, 5, 2)

  def makeDerivedHalfAdderNetwork(base: NetworkConfig): NetworkConfig =
    randomlyDeriveNetwork(base)


  val startMs = System.currentTimeMillis()
  var curr = makeHalfAdderNetwork
  var currFitness = computeFitness(curr)
  val maxIterations = 10_000_000
  var iterations = 0
  while iterations < maxIterations do {
    iterations += 1
    if (1 == iterations % 1_000_000) {
      val pct = 100.0 * iterations / maxIterations
      println(f"@ $iterations (/$maxIterations, ${pct}%4.1f%%) currFitness = $currFitness%11.5g ...")
    }
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
