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

  object Bias       { def apply(value: Double): Bias       = value }
  object Weight     { def apply(value: Double): Weight     = value }
  object Activation { def apply(value: Double): Activation = value }

  import scala.annotation.targetName
  extension (raw: Bias)       @targetName("Bias_raw")       def raw: Double = raw
  extension (raw: Weight)     @targetName("Weight_raw")     def raw: Double = raw
  extension (raw: Activation) @targetName("Activation_raw") def raw: Double = raw

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
}