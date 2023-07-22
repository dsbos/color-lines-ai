package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.ArrayTypes.{LayerActivations, LayerBiases, LayerWeights}
import com.us.dsb.colorlines.expl.nn.ScalarTypes.Activation

//?????? Update comments re ScalarTypes extraction
/**
 * Lower-level types for neural networks.
 */
object LowlevelTypes {

  //???????CONTINUE
  def computeLayerActivation(prevLayerActivations: LayerActivations,
                             thisLayerBiases     : LayerBiases,
                             thisLayerWeights    : LayerWeights
                            ): LayerActivations = {
    assert(thisLayerBiases.vector.size == thisLayerWeights.matrix.size)
    val thisLayerActivations =
      for {
        thisNeuronIdx <- thisLayerBiases.vector.indices
        thisNeuronBias = thisLayerBiases.vector(thisNeuronIdx)
        thisNeuronInputWeights = thisLayerWeights.matrix(thisNeuronIdx)
        _ = assert(prevLayerActivations.vector.size == thisNeuronInputWeights.size)
        sum = {
          // Optimization:  Avoids creating collection of products:  Was
          //   10-15% faster overall as of 2023-07-13:
          var sum2 = 0d
          for (prevNeuronIdx <- prevLayerActivations.vector.indices) {
            sum2 +=
                prevLayerActivations.vector(prevNeuronIdx).raw
                    * thisNeuronInputWeights(prevNeuronIdx).raw
          }
          sum2
        }
        rawAct = ActivationFunctions.standardLogisticFunction(sum + thisNeuronBias.raw)
        act = Activation(rawAct)
      } yield {
        act
      }
    LayerActivations(thisLayerActivations)
  }

}