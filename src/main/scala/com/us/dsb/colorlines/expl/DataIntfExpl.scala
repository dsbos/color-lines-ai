package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.ArrayTypes.LayerActivations
import com.us.dsb.colorlines.expl.nn.ScalarTypes.{Activation, Bias, Weight, raw}

object DataIntfExpl extends App {

  // Model 1:  single flat trait, all indexing methods (no collections/objects), "manual" size/offset correpondence
  object Model1:
    trait NeuralNetworkParametersIntf:
      def getInputNeuronCount          : Int  //????? "activations"? "inputs"?
      def getLayerCount                : Int
      def getNeuronCount(layerNum: Int): Int
      def getBias       (layerNum: Int)(neuronNum: Int)                    : Bias
      def getWeight     (layerNum: Int)(neuronNum: Int)(predNeuronNum: Int): Weight

  // Model 2:  Like Model 1 except with layer subobject (still no collections, etc.)
  object Model2:
    trait LayerParametersIntf:
      def getInputNeuronCount: Int
      def getNeuronCount     : Int
      def getBias  (neuronNum: Int): Bias
      def getWeight(neuronNum: Int)(predNeuronNum: Int): Weight
    trait NeuralNetworkParametersIntf:
      def getInputNeuronCount: Int  //?????? have redundantly or get via first layer?
      def getLayerCount      : Int
      def getLayer(layerNum: Int): LayerParametersIntf

  // Model 3:  Like model 2 except collection for layer subobjects (only)
  object Model3:
    trait LayerParametersIntf:
      def getInputNeuronCount: Int
      def getNeuronCount     : Int
      def getBias  (neuronNum: Int): Bias
      def getWeight(neuronNum: Int)(predNeuronNum: Int): Weight
    trait NeuralNetworkParametersIntf:
      def getInputNeuronCount: Int  //?????? have redundantly or get via first layer?
      def getLayers: IndexedSeq[LayerParametersIntf]

  // Model 4:  Typical collections/objects; two levels, biases separate from (2-D) weights; (no indexing methods)
  object Model4:
    trait LayerParametersIntf:
      def getInputNeuronCount: Int  //?????? have redundantly or get via bias or weight collections?
      def getNeuronCount     : Int  //?????? have redundantly or get via bias or weight collections?
      def getBiases : IndexedSeq[Bias]
      def getWeights: IndexedSeq[IndexedSeq[Weight]]
    trait NeuralNetworkParametersIntf:
      def getInputNeuronCount: Int      //?????? have redundantly or get via first layer?
      def getLayers: IndexedSeq[LayerParametersIntf]  // (or Seq)

  // Model 5:  Like Model 5 except with three levels--neurons with 1-D weights and bias
  object Model5:
    trait NeuronParametersIntf:
      def getInputNeuronCount: Int  //?????? what about redundancy and about proximity?
      def getBias   : Bias
      def getWeights: IndexedSeq[Weight]
    trait LayerParametersIntf:
      def getInputNeuronCount: Int
      def getNeuronCount     : Int
      def getNeuronsParameters: IndexedSeq[NeuronParametersIntf]
    trait NeuralNetworkParametersIntf:
      def getInputNeuronCount: Int  // Have redundantly to reduce digging down by client
      def getLayers: IndexedSeq[LayerParametersIntf]  // (or Seq)

  object Temp {

    def computeNeuronActivation1ViaSeqs(inputActivations: LayerActivations,
                                        neuronInputWeights: IndexedSeq[Weight],
                                        neuronBias: Bias
                                       ): Activation = {
      require(inputActivations.vector.size == neuronInputWeights.size,
              s"inputActivations.vector.size = ${inputActivations.vector.size}"
                  +  s" != neuronInputWeights.size = ${neuronInputWeights.size}")
      var sumAccum: Double = 0
      for (inputIdx <- inputActivations.vector.indices) {
        sumAccum += inputActivations.vector(inputIdx).raw
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
      var sumAccum: Double = 0
      for (inputIdx <- 0 until inputSize) {
        sumAccum += inputActivations(inputIdx).raw * neuronInputWeights(inputIdx).raw
      }
      val rawAct = ActivationFunctions.standardLogisticFunction(sumAccum + neuronBias.raw)
      Activation(rawAct)
    }

    val nn1: Model1.NeuralNetworkParametersIntf = ???
    val nn2: Model2.NeuralNetworkParametersIntf = ???
    val nn3: Model3.NeuralNetworkParametersIntf = ???
    val nn4: Model4.NeuralNetworkParametersIntf = ???
    val nn5: Model5.NeuralNetworkParametersIntf = ???

    val inputActivations: LayerActivations = ???


    val outputActivations1: LayerActivations =
      (0 until nn1.getLayerCount).foldLeft(inputActivations) { (inActs, layerIndex) =>
        val layerNeuronBases = nn1.getBias(layerIndex)
        val layerNeuronWeights = nn1.getWeight(layerIndex)
        val outActs =
          (0 until nn1.getNeuronCount(layerIndex)).map { neuronIdx =>
            computeNeuronActivationViaFns(
              inputSize          = inActs.vector.size,
              inputActivations   = inActs.vector,
              neuronInputWeights = layerNeuronWeights(neuronIdx),
              neuronBias         = layerNeuronBases(neuronIdx)
              )
          }
        LayerActivations(outActs)
      }

    val outputActivations2: LayerActivations =
      (0 until nn2.getLayerCount).foldLeft(inputActivations) { (inActs, layerIndex) =>
        val layer = nn2.getLayer(layerIndex)
        val outActs =
          (0 until layer.getNeuronCount).map { neuronIdx =>
            computeNeuronActivationViaFns(
              inputSize          = inActs.vector.size,
              inputActivations   = inActs.vector,
              neuronInputWeights =  layer.getWeight(neuronIdx),
              neuronBias         = layer.getBias(neuronIdx)
              )
          }
        LayerActivations(outActs)
      }

    val outputActivations3: LayerActivations =
      nn3.getLayers.foldLeft(inputActivations) { (inActs, layer) =>
        val outActs =
          (0 until layer.getNeuronCount).map { neuronIdx =>
            computeNeuronActivationViaFns(
              inputSize          = inActs.vector.size,
              inputActivations   = inActs.vector,
              neuronInputWeights = layer.getWeight(neuronIdx),
              neuronBias         = layer.getBias(neuronIdx)
              )
          }
        LayerActivations(outActs)
      }

    val outputActivations4: LayerActivations =
      nn4.getLayers.foldLeft(inputActivations) { (inActs, layer) =>
        val outActs =
          (0 until layer.getNeuronCount).map { neuronIdx =>
            computeNeuronActivation1ViaSeqs(
              inputActivations   = inActs,
              neuronInputWeights = layer.getWeights(neuronIdx),
              neuronBias         = layer.getBiases(neuronIdx))
            // (OR:)
            computeNeuronActivationViaFns(
              inputSize          = inActs.vector.size,
              inputActivations   = inActs.vector,
              neuronInputWeights = layer.getWeights(neuronIdx),
              neuronBias         = layer.getBiases(neuronIdx))
          }
        LayerActivations(outActs)
      }

    val outputActivations5: LayerActivations =
      nn5.getLayers.foldLeft(inputActivations) { (inActs, layer) =>
        val outActs =
          layer.getNeuronsParameters.map { neuronData =>
            computeNeuronActivation1ViaSeqs(
              inputActivations   = inActs,
              neuronInputWeights = neuronData.getWeights,
              neuronBias         = neuronData.getBias)
            // (OR:)
            computeNeuronActivationViaFns(
              inputSize          = inActs.vector.size,
              inputActivations   = inActs.vector,
              neuronInputWeights = neuronData.getWeights,
              neuronBias         = neuronData.getBias)
          }
        LayerActivations(outActs)
      }
  }

}
