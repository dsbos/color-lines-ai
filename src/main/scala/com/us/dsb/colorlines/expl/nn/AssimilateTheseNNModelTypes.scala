package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn2.ActivationFunctions
import com.us.dsb.colorlines.expl.nn2.types.LowlevelTypes.{
  Activation, Bias, LayerActivations, Weight, raw}


object AssimilateTheseNNModelTypes extends App {

  object Temp {

    def computeNeuronActivationViaSeqs(inputActivations: LayerActivations,
                                       neuronInputWeights: Seq[Weight],
                                       neuronBias: Bias
                                      ): Activation = {
      require(inputActivations.vector.size == neuronInputWeights.size,
              s"inputActivations.vector.size = ${inputActivations.vector.size}"
                  +  s" != neuronInputWeights.size = ${neuronInputWeights.size}")
      var sumAccum: Double = 0  //?????? add bias here?
      for (inputIdx <- inputActivations.vector.indices) {
        sumAccum +=
            inputActivations.vector(inputIdx).raw
                * neuronInputWeights(inputIdx).raw
      }
      Activation(ActivationFunctions.standardLogisticFunction(sumAccum + neuronBias.raw))
    }

    def computeNeuronActivationViaFns(inputSize         : Int,  //?????? OR index range (from .indices)?
                                      inputActivations  : Int => Activation,
                                      neuronInputWeights: Int => Weight,
                                      neuronBias        : Bias   //?? () => Bias for symmetry?
                                     ): Activation = {
      // Note:  Can't check sizes.
      var sumAccum: Double = 0  //?????? add bias here?
      for (inputIdx <- 0 until inputSize) {
        println(s"inputIdx = $inputIdx")
        sumAccum += inputActivations(inputIdx).raw * neuronInputWeights(inputIdx).raw
      }
      val rawAct = ActivationFunctions.standardLogisticFunction(sumAccum + neuronBias.raw)
      Activation(rawAct)
    }
  }
}
