package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.ScalarTypes.{Activation, Bias, Weight}

/**
 * Common vector/matrix types.
 */
object ArrayTypes {

  case class LayerBiases(vector: IndexedSeq[Bias])

  /**
   * @param matrix
   * ... indexed by layer neuron offset, then by previous-layer neuron offset
   */
  case class LayerWeights(matrix: IndexedSeq[IndexedSeq[Weight]])

  case class LayerActivations(vector: IndexedSeq[Activation])

}
