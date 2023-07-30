package com.us.dsb.colorlines.expl.nn2

import com.us.dsb.colorlines.expl.nn2.types.LowlevelTypes.{
  Activation, Bias, LayerActivations, Weight, raw}

object ActivationComputation {

  type ActivationFunction = Double => Activation //??????? clean to Activation -> Activation

  def computeNeuronActivation(inputActivations : LayerActivations,
                              neuronInputWeights: Seq[Weight],
                              neuronBias        : Bias,
                              activationFunction: ActivationFunction
                             ): Activation = {
    require(inputActivations.vector.size == neuronInputWeights.size,
            s"inputActivations.vector.size = ${inputActivations.vector.size}"
                +  s" != neuronInputWeights.size = ${neuronInputWeights.size}")
    var sumAccum: Double = 0  //?????? add bias here?
    for (inputIdx <- inputActivations.vector.indices) {  //???????? update to fast "while" version
      sumAccum +=
          inputActivations.vector(inputIdx).raw
              * neuronInputWeights(inputIdx).raw
    }
    activationFunction(sumAccum + neuronBias.raw)
  }

}
