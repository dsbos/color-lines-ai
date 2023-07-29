package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.ArrayTypes.LayerActivations
import com.us.dsb.colorlines.expl.nn.ScalarTypes.{Activation, Bias, Weight, raw}

import scala.collection.immutable.ArraySeq

object DataIntfExpl extends App {

  // Naming:
  // - "Config":
  //   - to indicate ~static parameters (not including computed activations)
  //   - singular, to be pluralizable (vs. "parameters")

  // Model 1:  single flat trait, all indexing methods (no collections/objects), "manual" size/offset correpondence
  object Model1:
    trait NeuralNetworkConfig:
      def inputCount                 : Int
      def layerCount                 : Int
      def neuronCounts(layerNum: Int): Int
      def biases      (layerNum: Int)(neuronNum: Int)                    : Bias
      def weights     (layerNum: Int)(neuronNum: Int)(predNeuronNum: Int): Weight
  // Note:  Thpse are unclear re when all weights, layer's weights, or neuron's weights.

  //???????? CONTINUE with "def xx"--use in assertions and/or purge:

  // Model 2:  Like Model 1 except with layer subobject (still no collections, etc.)
  object Model2:
    trait LayerConfig:
      def xxinputCount : Int
      def neuronCount: Int
      def biases (neuronNum: Int): Bias
      def weights(neuronNum: Int)(predNeuronNum: Int): Weight
    trait NeuralNetworkConfig:
      def inputCount: Int  // Have redundantly to reduce digging down by client
      def layerCount: Int
      def layer(layerNum: Int): LayerConfig

  // Model 3:  Like model 2 except collection for layer subobjects (only)
  object Model3:
    trait LayerConfig:
      def xxinputCount : Int
      def neuronCount: Int
      def biases (neuronNum: Int): Bias
      def weights(neuronNum: Int)(predNeuronNum: Int): Weight
    trait NeuralNetworkConfig:
      def inputCount: Int  // Have redundantly to reduce digging down by client
      def layers: IndexedSeq[LayerConfig]

  // Model 4:  Typical collections/objects; two levels, biases separate from (2-D) weights; (no indexing methods)
  object Model4:
    trait LayerConfig:
      def xxinputCount : Int  //?????? have redundantly or get via bias or weight collections?
      def neuronCount: Int  //?????? have redundantly or get via bias or weight collections?
      def biases : IndexedSeq[Bias]
      def weights: IndexedSeq[IndexedSeq[Weight]]
    trait NeuralNetworkConfig:
      def inputCount: Int  // Have redundantly to reduce digging down by client
      def layers    : IndexedSeq[LayerConfig]  // (or Seq)

  // Model 5:  Like Model 5 except with three levels--neurons with 1-D weights and bias
  object Model5:
    trait NeuronConfig:
      def xxinputCount: Int  //?????? what about redundancy and about proximity?
      def bias      : Bias
      def weights   : IndexedSeq[Weight]
    trait LayerConfig:
      def xxinputCount : Int  // Have somewhat redundantly to "cover" all neurons (weights arrays)
      def neurons: IndexedSeq[NeuronConfig]
    trait NeuralNetworkConfig:
      def inputCount: Int  // Have redundantly to reduce digging down by client
      def layers: IndexedSeq[LayerConfig]  // (or Seq)

  object Temp {

    def computeNeuronActivationViaSeqs(inputActivations: LayerActivations,
                                       neuronInputWeights: IndexedSeq[Weight],
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
        sumAccum += inputActivations(inputIdx).raw * neuronInputWeights(inputIdx).raw
      }
      val rawAct = ActivationFunctions.standardLogisticFunction(sumAccum + neuronBias.raw)
      Activation(rawAct)
    }

    val nn1: Model1.NeuralNetworkConfig = ???
    val nn2: Model2.NeuralNetworkConfig = ???
    val nn3: Model3.NeuralNetworkConfig = ???
    val nn4: Model4.NeuralNetworkConfig = ???
    val nn5: Model5.NeuralNetworkConfig = ???

    def makeInputActivations(count: Int): LayerActivations =
      LayerActivations(ArraySeq.fill(count)(Activation(1.23)))


    val outputActivations1: LayerActivations = {
      val inputActivations = makeInputActivations(nn1.inputCount)
      (0 until nn1.layerCount).foldLeft(inputActivations) { (inActs, layerIndex) =>
        val layerNeuronBases   = nn1.biases(layerIndex)
        val layerNeuronWeights = nn1.weights(layerIndex)
        val outActs =
          (0 until nn1.neuronCounts(layerIndex)).map { neuronIdx =>
            computeNeuronActivationViaFns(
              inputSize          = inActs.vector.size,
              inputActivations   = inActs.vector,
              neuronInputWeights = layerNeuronWeights(neuronIdx),
              neuronBias         = layerNeuronBases(neuronIdx)
              )
          }
        LayerActivations(outActs)
      }
    }

    val outputActivations2: LayerActivations = {
      val inputActivations = makeInputActivations(nn2.inputCount)
      (0 until nn2.layerCount).foldLeft(inputActivations) { (inActs, layerIndex) =>
        val layer = nn2.layer(layerIndex)
        val outActs =
          (0 until layer.neuronCount).map { neuronIdx =>
            computeNeuronActivationViaFns(
              inputSize          = inActs.vector.size,
              inputActivations   = inActs.vector,
              neuronInputWeights = layer.weights(neuronIdx),
              neuronBias         = layer.biases(neuronIdx)
              )
          }
        LayerActivations(outActs)
      }
    }

    val outputActivations3: LayerActivations = {
      val inputActivations = makeInputActivations(nn3.inputCount)
      nn3.layers.foldLeft(inputActivations) { (inActs, layer) =>
        val outActs =
          (0 until layer.neuronCount).map { neuronIdx =>
            computeNeuronActivationViaFns(
              inputSize          = inActs.vector.size,
              inputActivations   = inActs.vector,
              neuronInputWeights = layer.weights(neuronIdx),
              neuronBias         = layer.biases(neuronIdx)
              )
          }
        LayerActivations(outActs)
      }
    }

    val outputActivations4: LayerActivations = {
      val inputActivations = makeInputActivations(nn4.inputCount)
      nn4.layers.foldLeft(inputActivations) { (inActs, layer) =>
        val outActs =
          (0 until layer.neuronCount).map { neuronIdx =>
            computeNeuronActivationViaSeqs(
              inputActivations   = inActs,
              neuronInputWeights = layer.weights(neuronIdx),
              neuronBias         = layer.biases(neuronIdx))
            // (OR:)
            computeNeuronActivationViaFns(
              inputSize          = inActs.vector.size,
              inputActivations   = inActs.vector,
              neuronInputWeights = layer.weights(neuronIdx),
              neuronBias         = layer.biases(neuronIdx))
          }
        LayerActivations(outActs)
      }
    }

    val outputActivations5: LayerActivations = {
      val inputActivations = makeInputActivations(nn5.inputCount)
      nn5.layers.foldLeft(inputActivations) { (inActs, layer) =>
        val outActs =
          layer.neurons.map { neuronData =>
            computeNeuronActivationViaSeqs(
              inputActivations   = inActs,
              neuronInputWeights = neuronData.weights,
              neuronBias         = neuronData.bias)
            // (OR:)
            computeNeuronActivationViaFns(
              inputSize          = inActs.vector.size,
              inputActivations   = inActs.vector,
              neuronInputWeights = neuronData.weights,
              neuronBias         = neuronData.bias)
          }
        LayerActivations(outActs)
      }
    }

  }

}
