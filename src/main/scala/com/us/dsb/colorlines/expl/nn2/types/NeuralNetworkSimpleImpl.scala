package com.us.dsb.colorlines.expl.nn2.types

import com.us.dsb.colorlines.expl.nn2.types.NeuralNetworkReadView.{
  LayerConfig, NetworkConfig, NeuronConfig}
import com.us.dsb.colorlines.expl.nn2.types.ScalarTypes.{Bias, Weight}

/**
 * Case classes for simple/direct instantiable model.
 */
object NeuralNetworkSimpleImpl {

  case class Neuron(override val inputCount: Int,
                    override val bias      : Bias,
                    override val weights   : Seq[Weight]
                   ) extends NeuronConfig {
    require(weights.size == inputCount,
            s"weights.size = ${weights.size} != $inputCount = inputCount")
  }

  case class Layer(override val inputCount: Int,
                   override val neurons   : Seq[Neuron]  //?????? impl. Neuron or intf. NeuronConfig?
                  ) extends LayerConfig {
    require(neurons.forall(n => n.inputCount == inputCount),
            s"inputCount = $inputCount, neurons.map(_.xxinputCount) = ${neurons.map(_.inputCount)}")
  }

  case class Network(override val inputCount: Int,
                     override val layers    : Seq[Layer]  //?????? impl. Layer or intf. LayerConfig?
                    ) extends NetworkConfig {
    require(
      layers.map(_.inputCount) == inputCount +: layers.map(_.neurons.size).dropRight(1),
      s"A layer input count(s) doesn't match predecessor size: "
          + s"layers.map(_.inputCount) = ${layers.map(_.inputCount)}"
          + s" != ${inputCount +: layers.dropRight(1).map(_.neurons.size)}"
          + s" = inputCount +: layers.dropRight(1).map(_.neurons.size)")
  }
}
