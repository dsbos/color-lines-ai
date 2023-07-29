package com.us.dsb.colorlines.expl.nn2.types

import scala.annotation.targetName

// Note;  In separate scope from array code for better opaqueness in array-
//   related code.
/**
 * Common scalar/numeric types.
 */
// ???? TODO:  Revisit name; "core"? "low-level"? "numeric"? (maybe not nec'ly all scalar)
object ScalarTypes {

  opaque type Bias       = Double
  opaque type Weight     = Double
  opaque type Activation = Double

  object Bias       { def apply(value: Double): Bias       = value }
  object Weight     { def apply(value: Double): Weight     = value }
  object Activation { def apply(value: Double): Activation = value }

  // (Note;  I wanted to avoid needing import Bias.* and siblings; hence @targetName:)
  extension (raw: Bias)       @targetName("Bias_raw")       def raw: Double = raw
  extension (raw: Weight)     @targetName("Weight_raw")     def raw: Double = raw
  extension (raw: Activation) @targetName("Activation_raw") def raw: Double = raw
}
