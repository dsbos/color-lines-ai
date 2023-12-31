package com.us.dsb.colorlines.expl.nn2.types

import com.us.dsb.colorlines.expl.nn2.types.LowlevelTypes.{Bias, Weight}

/**
 * Read-only API for parameters (no stored activations) of neural network.
 */
object NeuralNetworkReadView {

  // Naming:
  // - "Config":
  //   - to indicate ~static parameters (not including computed activations)
  //   - singular, to be pluralizable (vs. "parameters")

  trait NeuronConfig:
    def inputCount: Int
    def bias      : Bias
    def weights   : Seq[Weight]  // (caller should pass IndexedSeq)

  trait LayerConfig:
    def inputCount: Int                // Have somewhat redundantly to "cover" all neurons (weights arrays)
    def neurons   : Seq[NeuronConfig]  // (caller should pass IndexedSeq)

  trait NetworkConfig:
    def inputCount: Int               // Have redundantly to reduce digging down by client
    def layers    : Seq[LayerConfig]  // (caller should pass IndexedSeq)
    //?????? what about outputSize/-Count?; Have redundantly to reduce digging down by client?
    // (does client need/want to know size before executing, when it'll have output of size?

}
