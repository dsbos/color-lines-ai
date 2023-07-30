package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.types.ArrayTypes.OLDLayerParameters

object TypesToSort {
  case class OneHiddenTopology(inputLayerSize : Int,
                               hiddenLayerSize: Int,
                               outputLayerSize: Int)

  // ?????? TODO:  Assimilate model from DataIntfExpl/NameThis
  trait OLDOneHiddenNeuralNetworkWeightsAndBiases {
    val hiddenLayer: OLDLayerParameters
    val outputLayer: OLDLayerParameters

    def inputSize: Int = hiddenLayer.inputSize
  }

}
