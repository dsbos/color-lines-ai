package com.us.dsb.colorlines.expl.nn2

import com.us.dsb.colorlines.expl.nn2.ActivationComputation.{
  ActivationFunction, computeNetworkOutputActivation}
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
      val `rand_-x_to_x`: Double = `rand_-1_to_1` * 20 // 5 // 20 //????
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

  def makeHalfAdderNetwork: NetworkConfig = makeBaseNetwork(3, 5, 2)

  def makeDerivedHalfAdderNetwork(base: NetworkConfig): NetworkConfig =
    randomlyDeriveNetwork(base)

  HalfAdderCommon.run(makeHalfAdderNetwork,
                      base => makeDerivedHalfAdderNetwork(base))
}
