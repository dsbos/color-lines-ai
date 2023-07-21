package com.us.dsb.colorlines.expl.nn

/**
 * Lower-level types for neural networks.
 */
object LowlevelTypes {

  //////////
  // Scalar/numeric types:

  opaque type Bias       = Double
  opaque type Weight     = Double
  opaque type Activation = Double

  object Bias:
    def apply(value: Double)           : Bias       = value
    extension (raw: Bias)       def raw: Double     = raw

  object Weight:
    def apply(value: Double)           : Weight     = value
    extension (raw: Weight)     def raw: Double     = raw
  object Activation:
    def apply(value: Double)           : Activation = value
    extension (raw: Activation) def raw: Double     = raw

  //////////
  // Weight/etc. vector and matrix types:

  // ?????? TODO:  Revisit names "vector" and "matrix"; shorter? semantic (esp. 2-D weights)?

  case class LayerBiases(vector: IndexedSeq[Bias])
  /**
   * @param matrix
   *   ... indexed by layer neuron offset, then by previous-layer neuron offset
   */
  case class LayerWeights(matrix: IndexedSeq[IndexedSeq[Weight]])
  case class LayerActivations(vector: IndexedSeq[Activation])

  def computeLayerActivation(prevLayerActivations: LayerActivations,
                             thisLayerBiases     : LayerBiases,
                             thisLayerWeights    : LayerWeights
                            ): LayerActivations = {
    // Note weird extension/import behavior:   Importing Bias.raw makes
    //   Weight.raw and Activation.raw avialable too.  Normal?  Compiler bug?
    import Bias.raw
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