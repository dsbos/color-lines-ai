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
      def biases      (layerNum: Int)(neuronNum: Int)               : Bias
      def weights     (layerNum: Int)(neuronNum: Int)(inputNum: Int): Weight
  // Note:  Thpse are unclear re when all weights, layer's weights, or neuron's weights.

  //???????? CONTINUE with "def xx"--use in assertions and/or purge:

  // Model 2:  Like Model 1 except with layer subobject (still no collections, etc.)
  object Model2:
    trait LayerConfig:
      def xxinputCount  : Int
      def neuronCount: Int
      def biases (neuronNum: Int)               : Bias
      def weights(neuronNum: Int)(inputNum: Int): Weight
    trait NeuralNetworkConfig:
      def inputCount: Int  // Have redundantly to reduce digging down by client
      def layerCount: Int
      def layer(layerNum: Int): LayerConfig

  // Model 3:  Like model 2 except collection for layer subobjects (only)
  object Model3:
    trait LayerConfig:
      def xxinputCount : Int
      def neuronCount: Int
      def biases (neuronNum: Int)               : Bias
      def weights(neuronNum: Int)(inputNum: Int): Weight
    trait NeuralNetworkConfig:
      def inputCount: Int  // Have redundantly to reduce digging down by client
      def layers: Seq[LayerConfig]  // (caller should pass IndexedSeq)

  // Model 4:  Typical collections/objects; two levels, biases separate from (2-D) weights; (no indexing methods)
  object Model4:
    trait LayerConfig:
      def xxinputCount : Int  //?????? have redundantly or get via bias or weight collections?
      def neuronCount: Int  //?????? have redundantly or get via bias or weight collections?
      def biases : Seq[Bias]            // (caller should pass IndexedSeq)
      def weights: Seq[Seq[Weight]]     // (caller should pass IndexedSeq)
    trait NeuralNetworkConfig:
      def inputCount: Int               // Have redundantly to reduce digging down by client
      def layers    : Seq[LayerConfig]  // (caller should pass IndexedSeq)

  // Model 5:  Like Model 5 except with three levels--neurons with 1-D weights and bias
  object Model5:
    trait NeuronConfig:
      def xxinputCount: Int  //?????? what about redundancy and about proximity?
      def bias      : Bias
      def weights   : Seq[Weight]     // (caller should pass IndexedSeq)
    trait LayerConfig:
      def inputCount : Int            // Have somewhat redundantly to "cover" all neurons (weights arrays)
      def neurons: Seq[NeuronConfig]  // (caller should pass IndexedSeq)
    trait NeuralNetworkConfig:
      def inputCount: Int             // Have redundantly to reduce digging down by client
      def layers: Seq[LayerConfig]    // (caller should pass IndexedSeq)

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

//    val nn1: Model1.NeuralNetworkConfig = ???
//    val nn2: Model2.NeuralNetworkConfig = ???
//    val nn3: Model3.NeuralNetworkConfig = ???
//    val nn4: Model4.NeuralNetworkConfig = ???
//    val nn5: Model5.NeuralNetworkConfig = ???

    val nn1 =
      new Model1.NeuralNetworkConfig {
        override def inputCount: Int = nn5.inputCount
        override def layerCount: Int = nn5.layers.size
        override def neuronCounts(layerNum: Int): Int = nn5.layers(layerNum).neurons.size
        override def biases(layerNum: Int)
                           (neuronNum: Int): Bias = nn5.layers(layerNum).neurons(neuronNum).bias
        override def weights(layerNum : Int)
                            (neuronNum: Int)
                            (inputNum : Int): Weight = nn5.layers(layerNum).neurons(neuronNum).weights(inputNum)
      }
    val nn2 =
      new Model2.NeuralNetworkConfig {
        override def inputCount: Int = nn5.inputCount
        override def layerCount: Int = nn5.layers.size
        override def layer(layerNum: Int): Model2.LayerConfig = {
          case class LayerConfigImpl(layer: Model5Impl.Layer)
              extends Model2.LayerConfig {
            override def xxinputCount: Int = ???
            override def neuronCount: Int = layer.neurons.size
            override def biases(neuronNum: Int): Bias = layer.neurons(neuronNum).bias
            override def weights(neuronNum: Int)
                                (inputNum : Int): Weight = layer.neurons(neuronNum).weights(inputNum)
          }
          LayerConfigImpl(nn5.layers(layerNum))
        }
      }
    val nn3 =
      new Model3.NeuralNetworkConfig {
        override def inputCount: Int = nn5.inputCount

        override def layers: Seq[Model3.LayerConfig] = ???
      }
    val nn4 =
      new Model4.NeuralNetworkConfig {
        override def inputCount: Int = nn5.inputCount

        override def layers: Seq[Model4.LayerConfig] = ???
      }

    object Model5Impl {
      import Model5.*

      case class Neuron(override val xxinputCount: Int,
                        override val bias: Bias,
                        override val weights: Seq[Weight]) extends NeuronConfig {
        require(weights.size == xxinputCount,
                s"weights.size = ${weights.size} != $xxinputCount = xxinputCount")
      }

      case class Layer(override val inputCount: Int,
                       override val neurons: Seq[Neuron]  //?????? Neuron or NeuronConfig?
                      ) extends LayerConfig {
        require(neurons.forall( n => n.xxinputCount == inputCount),
                s"inputCount = $inputCount, neurons.map(_.xxinputCount) = ${neurons.map(_.xxinputCount)}")
      }

      case class NeuralNetwork(override val inputCount: Int,
                               override val layers: Seq[Layer]  //?????? Layer or LayerConfig?
                              ) extends NeuralNetworkConfig {
        println(s"layers.map(_.neurons.size = ${layers.map(_.neurons.size)}")
        println(s"layers.map(_.neurons.size.dropRight(1)) = ${layers.map(_.neurons.size).dropRight(1)}")
        println(s"inputCount +: layers.map(_.neurons.size.dropRight(1)) = ${inputCount +: layers.map(_.neurons.size).dropRight(1)}")
        println(s"layers.map(_.inputCount) = ${layers.map(_.inputCount)}")
        require(
          layers.map(_.inputCount) == inputCount +: layers.map(_.neurons.size).dropRight(1),
          s"A layer input count(s) doesn't match predecessor size: "
              + s"layers.map(_.inputCount) = ${layers.map(_.inputCount)}"
              + s" != ${inputCount +: layers.dropRight(1).map(_.neurons.size)}"
              + s" = inputCount +: layers.dropRight(1).map(_.neurons.size)"
          )
      }
    }

    case class Model5NeuralNetworkParametersImpl(inputCount: Int,
                                                ) extends Model5.NeuralNetworkConfig:
      //????override def inputCount: Int = ???

      override def layers: Seq[Model5.LayerConfig] = ???

    val nn5 = {
      import Model5Impl.*
      val inputCount = 2
      new NeuralNetwork(
        inputCount,
        Vector(
          Layer(inputCount,
                Vector(
                  Neuron(inputCount,
                         Bias(2.1),
                         Vector(Weight(23.101), Weight(23.102))),
                  Neuron(inputCount,
                         Bias(2.2),
                         Vector(Weight(23.201), Weight(23.202))),
                  Neuron(inputCount,
                         Bias(2.3),
                         Vector(Weight(23.301), Weight(23.302)))
                )),
          Layer(3,
                Vector(
                  Neuron(3,
                         Bias(2.4),
                         Vector(Weight(23.401), Weight(23.402), Weight(23.403)))
                ))
          )
        )
    }

    def makeInputActivations(count: Int): LayerActivations =
      LayerActivations(ArraySeq.fill(count)(Activation(1.23)))


    val outputActivations1: LayerActivations = {
      val inputActivations = makeInputActivations(nn1.inputCount)
      (0 until nn1.layerCount).foldLeft(inputActivations) { (inActs, layerIndex) =>
        println(s"layerIndex = $layerIndex")
        val layerNeuronBases   = nn1.biases(layerIndex)
        val layerNeuronWeights = nn1.weights(layerIndex)
        val outActs =
          (0 until nn1.neuronCounts(layerIndex)).map { neuronIdx =>
            println(s"neuronIdx = $neuronIdx")
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
  Temp
}
