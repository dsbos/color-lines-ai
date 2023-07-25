package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.ArrayTypes.LayerParameters

object TypesToSort {
  case class OneHiddenTopology(inputLayerSize : Int,
                               hiddenLayerSize: Int,
                               outputLayerSize: Int)

  trait OneHiddenNeuralNetworkWeightsAndBiases {
    val hiddenLayer: LayerParameters
    val outputLayer: LayerParameters

    def inputSize: Int = hiddenLayer.inputSize
  }

}
