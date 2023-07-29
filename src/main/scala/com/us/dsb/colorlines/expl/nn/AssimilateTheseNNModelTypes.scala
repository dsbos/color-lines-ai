package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.types.ArrayTypes.LayerActivations
import com.us.dsb.colorlines.expl.nn2.types.ScalarTypes.{Activation, Bias, Weight, raw}
import com.us.dsb.colorlines.expl.nn2.types.NeuralNetworkReadView.{
  LayerConfig, NetworkConfig, NeuronConfig}

import scala.collection.immutable.ArraySeq

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

//    val nn1: Model1.NeuralNetworkConfig = ???
//    val nn2: Model2.NeuralNetworkConfig = ???
//    val nn3: Model3.NeuralNetworkConfig = ???
//    val nn4: Model4.NeuralNetworkConfig = ???
//    val nn5: Model5.NeuralNetworkConfig = ???

    object Model5Impl {


      case class Neuron(override val inputCount: Int,
                        override val bias: Bias,
                        override val weights: Seq[Weight]) extends NeuronConfig {
        require(weights.size == inputCount,
                s"weights.size = ${weights.size} != $inputCount = xxinputCount")
      }

      case class Layer(override val inputCount: Int,
                       override val neurons: Seq[Neuron]  //?????? Neuron or NeuronConfig?
                      ) extends LayerConfig {
        require(neurons.forall( n => n.inputCount == inputCount),
                s"inputCount = $inputCount, neurons.map(_.xxinputCount) = ${neurons.map(_.inputCount)}")
      }

      case class NeuralNetwork(override val inputCount: Int,
                               override val layers: Seq[Layer]  //?????? Layer or LayerConfig?
                              ) extends NetworkConfig {
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
                                                ) extends NetworkConfig:
      //????override def inputCount: Int = ???

      override def layers: Seq[LayerConfig] = ???

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
