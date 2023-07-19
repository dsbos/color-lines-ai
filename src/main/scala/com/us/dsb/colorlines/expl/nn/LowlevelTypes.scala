package com.us.dsb.colorlines.expl.nn

/**
 * Lower-level types for neural networks.
 */
object LowlevelTypes {
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

  // ?????? TODO:  Maybe include activation/etc. vectors and matrices.
}