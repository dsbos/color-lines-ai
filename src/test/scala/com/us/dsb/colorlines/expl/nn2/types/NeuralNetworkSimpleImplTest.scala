package com.us.dsb.colorlines.expl.nn2.types

import com.us.dsb.colorlines.expl.nn2.types.NeuralNetworkSimpleImpl.{
  Layer, Network, Neuron}
import com.us.dsb.colorlines.expl.nn2.types.LowlevelTypes.{
  Activation, Bias, LayerActivations, Weight}
import com.us.dsb.colorlines.expl.nn2.ActivationComputation.{
  ActivationFunction, computeNeuronActivation}
import com.us.dsb.colorlines.expl.nn2.ActivationFunctions

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import scala.collection.immutable.ArraySeq

class NeuralNetworkSimpleImplTest extends AnyFunSpec {

  it("") {

    val nn5 = {
      val inputCount = 2
      Network(
        inputCount,
        Vector(
          Layer(inputCount,
                Vector(
                  Neuron(inputCount,
                         Bias(0.00),
                         Vector(Weight(0.000), Weight(0.001))),
                  Neuron(inputCount,
                         Bias(0.01),
                         Vector(Weight(0.010), Weight(0.011))),
                  Neuron(inputCount,
                         Bias(0.02),
                         Vector(Weight(0.020), Weight(0.021)))
                )),
          Layer(3,
                Vector(
                  Neuron(3,
                         Bias(0.10),
                         Vector(Weight(0.100), Weight(0.101), Weight(0.103)))
                ))
          )
        )
    }

    def makeInputActivations(count: Int): LayerActivations =
      LayerActivations(ArraySeq.fill(count)(Activation(1.0)))

    def actFn: ActivationFunction =
      raw => Activation(ActivationFunctions.standardLogisticFunction(raw))

    val outputActivations: LayerActivations = {
      val inputActivations = makeInputActivations(nn5.inputCount)
      nn5.layers.foldLeft(inputActivations) { (inActs, layer) =>
        val outActs =
          layer.neurons.map { neuronData =>
            computeNeuronActivation(inputActivations   = inActs,
                                    weights            = neuronData.weights,
                                    bias               = neuronData.bias,
                                    activationFunction = actFn)
          }
        LayerActivations(outActs)
      }
    }
    println(s"outputActivations = $outputActivations")
    outputActivations.vector(0) shouldBe Activation(0.5632537687417511)
  }

}

