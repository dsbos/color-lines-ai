package com.us.dsb.colorlines.expl.nn

import com.us.dsb.colorlines.expl.nn.LowlevelTypes.{Activation, Bias, Weight}

import scala.util.Random

object HalfAdderRandomWeightsNNExpl extends App {
  
  case class OneHiddenTopology(inputLayerSize : Int,
                               hiddenLayerSize: Int,
                               outputLayerSize: Int)

  case class RandomOneHiddenNeuralNetworkWeightsAndBiases(topology: OneHiddenTopology) {
    private def randomWeight: Weight = {
      if true then {
        val `rand_0_to_1`: Double = Random.nextFloat()
        //?????? parameterize
        val `rand_-1_to_1`: Double = (`rand_0_to_1` - 0.5) * 2
        val `rand_-x_to_x`: Double = `rand_-1_to_1` * 20  //????
        Weight(`rand_-x_to_x`)
      }
      else {
        Weight(Random.nextGaussian() * 20)
      }
    }

    private def randomBias: Bias = Bias(randomWeight.raw)  // ?? same for now

    import topology.*
    //?????? wrap collection usages (opaque type? regular class?)
    val hiddenLayerBiases: IndexedSeq[Bias] =
      IndexedSeq.fill(hiddenLayerSize)(randomBias)
    val hiddenLayerInputWeights: IndexedSeq[IndexedSeq[Weight]] =  // indexed by hidden, then input
      IndexedSeq.fill(hiddenLayerSize)(IndexedSeq.fill(inputLayerSize)(randomWeight))
    val outputLayerBiases: IndexedSeq[Bias] =
      IndexedSeq.fill(outputLayerSize)(randomBias)
    val outputLayerInputWeights: IndexedSeq[IndexedSeq[Weight]] =  // indexed by output, then hidden
      IndexedSeq.fill(outputLayerSize)(IndexedSeq.fill(hiddenLayerSize)(randomWeight))
    print("")
  }

  /**
   * ... without explicit neuron objects (with arrays) ... or stored activations ...
   */
  case class RandomlyWeightedOneHiddenTopologyNeuralNetwork2(topology: OneHiddenTopology) {
    private val weightsAndBiases = RandomOneHiddenNeuralNetworkWeightsAndBiases(topology)

    def computeActivations(inputActivations: IndexedSeq[Activation]): IndexedSeq[Activation] = {

      def computeLayerActivation(prevLayerActs: IndexedSeq[Activation],
                                 thisLayerBiases: IndexedSeq[Bias],
                                 thisLayerWeights: IndexedSeq[IndexedSeq[Weight]]
                                ): IndexedSeq[Activation] = {
        assert(thisLayerBiases.size == thisLayerWeights.size)
        val thisLayerActivations =
          for {
            thisNeuronIdx <- thisLayerBiases.indices
            thisNeuronBias = thisLayerBiases(thisNeuronIdx)
            thisNeuronInputWeights = thisLayerWeights(thisNeuronIdx)
            //???assert(prevLayerActs.size == thisNeuronInputWeights.size)
            sum = {
              // Optimization:  Avoids creating collection of products:  Was
              //   10-15% faster overall as of 2023-07-13:
              var sum2 = 0d
              for (prevNeuronIdx <- prevLayerActs.indices) {
                sum2 += prevLayerActs(prevNeuronIdx).raw * thisNeuronInputWeights(prevNeuronIdx).raw
              }
              sum2
            }
            rawAct = ActivationFunctions.standardLogisticFunction(sum + thisNeuronBias.raw)
            act = Activation(rawAct)
          } yield {
            act
          }
        thisLayerActivations
      }

      val hiddenLayerActivations =
        computeLayerActivation(inputActivations,
                              weightsAndBiases.hiddenLayerBiases,
                              weightsAndBiases.hiddenLayerInputWeights)

      val outputLayerActivations =
        computeLayerActivation(hiddenLayerActivations,
                              weightsAndBiases.outputLayerBiases,
                              weightsAndBiases.outputLayerInputWeights)
      outputLayerActivations
    }
  }

  //?????? rename; longer; "evaluate" sounds like evaluating fitness, vs. evaluating
  //  next function; ** "execute"? "compute"? ~~"evaluate function"
  //?????? Maybe wrap Byte in Bit opaque type to limit to 0 and 1:
  //?????? Possibly wrap Double in something reflecting approximated "bitness":
  def eval(nw: RandomlyWeightedOneHiddenTopologyNeuralNetwork2,
           inputs: (Byte, Byte, Byte)
          ): (Double, Double) = {

    // ???? TODO:  Is there way to map tuple to collection?  Or have fixed length
    // on collection?  (Tuple inputs and outputs were for fixed lengths for half-adder.)
    val inputActivations = IndexedSeq(Activation(inputs._1),
                                      Activation(inputs._2),
                                      Activation(inputs._3))
    val outputActivations  = nw.computeActivations(inputActivations)
    (outputActivations(0).raw, outputActivations(1).raw)
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
