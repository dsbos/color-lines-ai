package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.types.ArrayTypes.LayerParameters

object TypesToSort {
  case class OneHiddenTopology(inputLayerSize : Int,
                               hiddenLayerSize: Int,
                               outputLayerSize: Int)

  // ?????? TODO:  Assimilate model from DataIntfExpl/NameThis
  trait OneHiddenNeuralNetworkWeightsAndBiases {
    val hiddenLayer: LayerParameters
    val outputLayer: LayerParameters

    def inputSize: Int = hiddenLayer.inputSize
  }

}
