package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.ArrayTypes.{LayerActivations, LayerBiases, LayerWeights}
import com.us.dsb.colorlines.expl.nn.ScalarTypes.{Activation, Bias, Weight, raw}

import scala.util.Random

object HalfAdderAnnealedWeightsNNExpl extends App {

  case class OneHiddenTopology(inputLayerSize : Int,
                               hiddenLayerSize: Int,
                               outputLayerSize: Int)

  trait OneHiddenNeuralNetworkWeightsAndBiases {
    val hiddenLayerBiases: LayerBiases
    val hiddenLayerInputWeights: LayerWeights
    val outputLayerBiases: LayerBiases
    val outputLayerInputWeights: LayerWeights  // indexed by output, then hidden
  }

  private def randomSomething: Double = {
    if true then {
      val `rand_0_to_1`: Double = Random.nextFloat()
      //?????? parameterize
      val `rand_-1_to_1`: Double = (`rand_0_to_1` - 0.5) * 2
      val `rand_-x_to_x`: Double = `rand_-1_to_1` * 2 // 20 // 5 // 20 //????
      //println(s"randomSomething: ${`rand_-x_to_x`}")
      `rand_-x_to_x`
    }
    else {
      Random.nextGaussian() * 20
    }
  }

  private def randomWeight: Weight = Weight(randomSomething) // ?? same for now
  private def randomBias: Bias = Bias(randomSomething) // ?? same for now

  case class RandomOneHiddenNeuralNetworkWeightsAndBiases(topology: OneHiddenTopology)
      extends OneHiddenNeuralNetworkWeightsAndBiases{
    import topology.*
    //?????? wrap collection usages (value type? opaque type? regular class?)
    override val hiddenLayerBiases: LayerBiases =
      LayerBiases.fill(hiddenLayerSize)(Bias(0))
    override val hiddenLayerInputWeights: LayerWeights =
      LayerWeights.fill(hiddenLayerSize, inputLayerSize)(Weight(0))
    override val outputLayerBiases: LayerBiases =
      LayerBiases.fill(outputLayerSize)(Bias(0))
    override val outputLayerInputWeights: LayerWeights =
      LayerWeights.fill(outputLayerSize, hiddenLayerSize)(Weight(0))
//    println(s"Random: ... hiddenLayerBiases = $hiddenLayerBiases")
//    println(s"Random: ... hiddenLayerInputWeights = $hiddenLayerInputWeights")
//    println(s"Random: ... outputLayerBiases = $outputLayerBiases")
//    println(s"Random: ... outputLayerInputWeights = $outputLayerInputWeights")
//    print("")
  }

  class DerivedRandomOneHiddenNeuralNetworkWeightsAndBiases(
                                                                 base: OneHiddenNeuralNetworkWeightsAndBiases) extends OneHiddenNeuralNetworkWeightsAndBiases {
    val reductionFactor = 10.0
    override val hiddenLayerBiases: LayerBiases =
      LayerBiases(base.hiddenLayerBiases.vector.map(b => Bias(b.raw + randomBias.raw / 10)))
    //println(s"DerivedOneHiddenNeuralNetworkWeightsAndBiases: hiddenLayerBiases = $hiddenLayerBiases")
    override val hiddenLayerInputWeights: LayerWeights =
      LayerWeights(
        base.hiddenLayerInputWeights.matrix.map { weights =>
          weights.map( w => Weight(w.raw + randomWeight.raw / 10))
        })
    override val outputLayerBiases: LayerBiases =
      LayerBiases(base.outputLayerBiases.vector.map(b => Bias(b.raw + randomBias.raw / 10)))
    override val outputLayerInputWeights: LayerWeights =
      LayerWeights(
        base.outputLayerInputWeights.matrix.map { weights =>
          weights.map(w => Weight(w.raw + randomWeight.raw / 10))
        })
//    println(s"Derived: ... base.hiddenLayerBiases = $hiddenLayerBiases")
//    println(s"Derived: ...      hiddenLayerBiases = ${base.hiddenLayerBiases}")
//    println(s"Derived: ... base.hiddenLayerInputWeights = $hiddenLayerInputWeights")
//    println(s"Derived: ...      hiddenLayerInputWeights = ${base.hiddenLayerInputWeights}")
//    println(s"Derived: ... base.outputLayerBiases = $outputLayerBiases")
//    println(s"Derived: ...      outputLayerBiases = ${base.outputLayerBiases}")
//    println(s"Derived: ... base.outputLayerInputWeights = $outputLayerInputWeights")
//    println(s"Derived: ...      outputLayerInputWeights = ${base.outputLayerInputWeights}")
//    print("")
  }


  /**
   * ... without explicit neuron objects (with arrays) ... or stored activations ...
   */
  abstract class OneHiddenTopologyNeuralNetwork(val topology: OneHiddenTopology) {
    def weightsAndBiases: OneHiddenNeuralNetworkWeightsAndBiases

    def xxcomputeActivations(inputActivations: LayerActivations): LayerActivations = {

      val hiddenLayerActivations =
        ArrayTypes.computeLayerActivation(inputActivations,
                                          weightsAndBiases.hiddenLayerBiases,
                                          weightsAndBiases.hiddenLayerInputWeights)

      val outputLayerActivations =
        ArrayTypes.computeLayerActivation(hiddenLayerActivations,
                                          weightsAndBiases.outputLayerBiases,
                                          weightsAndBiases.outputLayerInputWeights)
      outputLayerActivations
    }
  }

  case class RandomlyWeightedOneHiddenTopologyNeuralNetwork(override val topology: OneHiddenTopology)
      extends OneHiddenTopologyNeuralNetwork(topology) {
    override val weightsAndBiases = RandomOneHiddenNeuralNetworkWeightsAndBiases(topology)
  }

  case class DerivedRandomlyWeightedOneHiddenTopologyNeuralNetwork(
                                                                         base: OneHiddenTopologyNeuralNetwork
                                                                     )
      extends OneHiddenTopologyNeuralNetwork(base.topology) {
    override val weightsAndBiases = DerivedRandomOneHiddenNeuralNetworkWeightsAndBiases(base.weightsAndBiases)
  }


  //?????? rename; longer; "evaluate" sounds like evaluating fitness, vs. evaluating
  //  next function; ** "execute"? "compute"? ~~"evaluate function"
  //?????? Maybe wrap Byte in Bit opaque type to limit to 0 and 1:
  //?????? Possibly wrap Double in something reflecting approximated "bitness":
  def eval(nw: OneHiddenTopologyNeuralNetwork,
           inputs: (Byte, Byte, Byte)
          ): (Double, Double) = {

    // ???? TODO:  Is there way to map tuple to collection?  Or have fixed length
    // on collection?  (Tuple inputs and outputs were for fixed lengths for half-adder.)
    val inputActivations = LayerActivations(IndexedSeq(Activation(inputs._1),
                                                       Activation(inputs._2),
                                                       Activation(inputs._3)))  //??????
    //println(s"inputActivations = $inputActivations")
    val outputActivations  = nw.xxcomputeActivations(inputActivations)
    //println(s"outputActivations = $outputActivations")
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
  def computeFitness(nw: OneHiddenTopologyNeuralNetwork): Double = {
    val fitness =
      cases.map { case ((a1, a2, a3), (c, s)) =>
        val nnOutput = eval(nw, (a1, a2, a3))
        val cError = nnOutput._1 - c
        val sError = nnOutput._2 - s
        val error = cError * cError + sError * sError
        val caseFitness = -error
        caseFitness
      }.sum
    //println(s"fitness = $fitness")
    fitness
  }

  def makeHalfAdderNetwork: OneHiddenTopologyNeuralNetwork =
    RandomlyWeightedOneHiddenTopologyNeuralNetwork(OneHiddenTopology(3, 4, 2))

  def makeDerivedHalfAdderNetwork(base: OneHiddenTopologyNeuralNetwork): OneHiddenTopologyNeuralNetwork =
    DerivedRandomlyWeightedOneHiddenTopologyNeuralNetwork(base)


  val startMs = System.currentTimeMillis()
  var curr = makeHalfAdderNetwork
  var currFitness = computeFitness(curr)
  val maxIterations = 100_000_000
  var iterations = 0
  while iterations < maxIterations do {
    iterations += 1
    if (1 == iterations % 1_000_000) {
      val pct = 100.0 * iterations / maxIterations
      println(f"@ $iterations (/$maxIterations, ${pct}%4.1f%%) currFitness = $currFitness%11.5g ...")
    }
    //??????val cand = makeHalfAdderNetwork
    val cand = makeDerivedHalfAdderNetwork(curr)
    val candFitness = computeFitness(cand)

    if candFitness > currFitness then {
      println(f"@ $iterations: base: $currFitness%11.5g -> cand: $candFitness%11.5g")
      cases.foreach { case ((a1, a2, a3), (c, s)) =>
        val nnOutput = eval(cand, (a1, a2, a3))
        println(f"$a1 + $a2 + $a3 = $c $s: ${nnOutput._1}%11.5g, ${nnOutput._2}%11.5g"
                    + f";  ∆c = ${nnOutput._1 - c}%11.5g"
                    + f", ∆s = ${nnOutput._2 - s}%11.5g")
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
