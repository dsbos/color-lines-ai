package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.ArrayTypes.{
  LayerActivations, LayerBiases, LayerWeights}
import com.us.dsb.colorlines.expl.nn.ScalarTypes.{
  Activation, Bias, Weight}

import scala.util.Random

object HalfAdderRandomWeightsNNExpl extends App {
  
  case class OneHiddenTopology(inputLayerSize : Int,
                               hiddenLayerSize: Int,
                               outputLayerSize: Int)

  case class RandomOneHiddenNeuralNetworkWeightsAndBiases(topology: OneHiddenTopology) {
    private def randomSomething: Double = {
      if true then {
        val `rand_0_to_1`: Double = Random.nextFloat()
        //?????? parameterize
        val `rand_-1_to_1`: Double = (`rand_0_to_1` - 0.5) * 2
        val `rand_-x_to_x`: Double = `rand_-1_to_1` * 20  //????
        //println(s"randomSomething: ${`rand_-x_to_x`}")
        `rand_-x_to_x`
      }
      else {
        Random.nextGaussian() * 20
      }
    }

    private def randomWeight: Weight = Weight(randomSomething)  // ?? same for now
    private def randomBias: Bias = Bias(randomSomething)  // ?? same for now

    import topology.*
    val hiddenLayerBiases: LayerBiases =
      LayerBiases(IndexedSeq.fill(hiddenLayerSize)(randomBias))
    val hiddenLayerInputWeights: LayerWeights =
      LayerWeights(IndexedSeq.fill(hiddenLayerSize)(IndexedSeq.fill(inputLayerSize)(randomWeight)))
    val outputLayerBiases: LayerBiases =
      LayerBiases(IndexedSeq.fill(outputLayerSize)(randomBias))
    val outputLayerInputWeights: LayerWeights =
      LayerWeights(IndexedSeq.fill(outputLayerSize)(IndexedSeq.fill(hiddenLayerSize)(randomWeight)))
    print("")
  }

  /**
   * ... without explicit neuron objects (with arrays) ... or stored activations ...
   */
  case class RandomlyWeightedOneHiddenTopologyNeuralNetwork2(topology: OneHiddenTopology) {
    private val weightsAndBiases = RandomOneHiddenNeuralNetworkWeightsAndBiases(topology)

    def computeActivations(inputActivations: LayerActivations): LayerActivations = {
      val hiddenLayerActivations =
        LowlevelTypes.computeLayerActivation(inputActivations,
                                             weightsAndBiases.hiddenLayerBiases,
                                             weightsAndBiases.hiddenLayerInputWeights)
      val outputLayerActivations =
        LowlevelTypes.computeLayerActivation(hiddenLayerActivations,
                                             weightsAndBiases.outputLayerBiases,
                                             weightsAndBiases.outputLayerInputWeights)
      outputLayerActivations
    }
  }

  //?????? rename; longer; "evaluate" sounds like evaluating fitness, vs. evaluating
  //  next function; ** "execute"? "compute output"? ~~"evaluate function"? "calculate output"?
  //  ("transform"?)
  //?????? Maybe wrap Byte in Bit opaque type to limit to 0 and 1:
  //?????? Possibly wrap Double in something reflecting approximated "bitness":
  def eval(nw: RandomlyWeightedOneHiddenTopologyNeuralNetwork2,
           inputs: (Byte, Byte, Byte)
          ): (Double, Double) = {

    // ???? TODO:  Is there way to map tuple to collection?  Or have fixed length on
    //   collection?  (Tuple inputs and outputs are for fixed lengths for half-adder.)
    // ???? TODO:  Should LayerActivations have var-args ~constructor?
    val inputActivations = LayerActivations(IndexedSeq(Activation(inputs._1),
                                                       Activation(inputs._2),
                                                       Activation(inputs._3)))
    val outputActivations  = nw.computeActivations(inputActivations)
    (outputActivations.vector(0).raw,
        outputActivations.vector(1).raw)
  }

  val cases: List[((Byte, Byte, Byte), (Byte, Byte))] = {
    List( // 3 bits to add -> 2-bit sum (high bit, low bit)
          (0, 0, 0) -> (0, 0),
          (0, 0, 1) -> (0, 1),
          (0, 1, 0) -> (0, 1),
          (0, 1, 1) -> (1, 0),
          (1, 0, 0) -> (0, 1),
          (1, 0, 1) -> (1, 0),
          (1, 1, 0) -> (1, 0),
          (1, 1, 1) -> (1, 1),
          )
        .map { case ((a1, a2, a3), (c, s)) =>
          ((a1.toByte, a2.toByte, a3.toByte), (c.toByte, s.toByte))
        }
  }

  // ?? TODO: _Possibly_ wrap Double in some opaque fitness type:
  def computeFitness(nw: RandomlyWeightedOneHiddenTopologyNeuralNetwork2): Double = {
    val fitness =
      cases.map { case ((a1, a2, a3), (c, s)) =>
        val nnOutput = eval(nw, (a1, a2, a3))
        val cError = nnOutput._1 - c
        val sError = nnOutput._2 - s
        val error = cError * cError + sError * sError
        val caseFitness = -error
        caseFitness
      }.sum
    fitness
  }

  def makeHalfAdderNetwork: RandomlyWeightedOneHiddenTopologyNeuralNetwork2 =
    RandomlyWeightedOneHiddenTopologyNeuralNetwork2(OneHiddenTopology(3, 4, 2))

  val startMs = System.currentTimeMillis()
  var curr = makeHalfAdderNetwork
  var currFitness = computeFitness(curr)
  val maxIterations = 100_000_000
  var iterations = 0
  while iterations < maxIterations do {
    iterations += 1
    if (1 == iterations % 1_000_000) {
      val pct = 100.0 * iterations / maxIterations
      println(f"@ $iterations (/$maxIterations, ${pct}%4.1f%%) currFitness = $currFitness%8.5f ...")
    }
    val cand = makeHalfAdderNetwork
    val candFitness = computeFitness(cand)

    if candFitness > currFitness then {
      println(f"@ $iterations: base: $currFitness%8.5f -> cand: $candFitness%8.5f")
      cases.foreach { case ((a1, a2, a3), (c, s)) =>
        val nnOutput = eval(cand, (a1, a2, a3))
        println(f"$a1 + $a2 + $a3 = $c $s: ${nnOutput._1}%8.5f, ${nnOutput._2}%8.5f"
                    + f";  ∆c = ${nnOutput._1 - c}%8.5f"
                    + f", ∆s = ${nnOutput._2 - s}%8.5f")
      }
      curr = cand
      currFitness = candFitness
    }
  }
  val endMs = System.currentTimeMillis()
  val durationMs = endMs - startMs
  println(s"for $iterations iterations, durationMs = $durationMs ms"
              + s" (${durationMs * 1.0 / iterations} ms/iteration)")
}
