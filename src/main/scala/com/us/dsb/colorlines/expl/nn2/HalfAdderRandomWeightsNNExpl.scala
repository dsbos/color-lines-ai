package com.us.dsb.colorlines.expl.nn2

import com.us.dsb.colorlines.expl.nn2.ActivationComputation.{ActivationFunction, computeNetworkOutputActivation}
import com.us.dsb.colorlines.expl.nn2.{ActivationFunctions, HalfAdderCommon}
import com.us.dsb.colorlines.expl.nn2.types.LowlevelTypes.{Activation, Bias, LayerActivations, Weight}
import com.us.dsb.colorlines.expl.nn2.types.NeuralNetworkSimpleImpl.{Layer, Network, Neuron}
import com.us.dsb.colorlines.expl.nn2.types.NeuralNetworkReadView.NetworkConfig

import scala.util.Random

object HalfAdderRandomWeightsNNExpl extends App {

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
  private def randomBias  : Bias   = Bias(randomSomething)    // ?? same for now

  /**
   * @param inputSize
   * @param layerSizes
   *   ...; (non-input layers)
   * @return
   */
  // ????? TODO:  Probably add random-bias/-weight function parameters
  def makeRandomNetwork(inputSize: Int, layerSizes: Int*): NetworkConfig = {
    def randomWeights(count: Int): Seq[Weight] = Vector.fill(count)(randomWeight)
    def randomNeurons(neuronCount: Int, inputSize: Int): Seq[Neuron] =
      Vector.fill(neuronCount)(
        Neuron(inputSize, randomBias, randomWeights(inputSize))
        )

    // Pair each non-input layer's size with predecessor's size:
    val layerAndInputSizePairs = layerSizes.zip(inputSize +: layerSizes.dropRight(1))
    val layers =
      layerAndInputSizePairs.map { (layerSize, inputSize) =>
        Layer(inputSize, randomNeurons(layerSize, inputSize))
      }
    Network(inputSize, layers)
  }

  def makeHalfAdderNetwork: NetworkConfig = makeRandomNetwork(3, 5, 2)

  HalfAdderCommon.run(makeHalfAdderNetwork,
                      _ => makeHalfAdderNetwork)
}
