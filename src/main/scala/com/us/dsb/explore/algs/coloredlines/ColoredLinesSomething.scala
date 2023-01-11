package com.us.dsb.explore.algs.coloredlines

//import scala.tools.nsc.doc.html.HtmlTags.Input


private[this] object ColoredLinesSomething extends App {


  private[this] trait NeuralStructure
  // - defines structure:
  //   - number and IDs/labels of input and output neurons
  //   - number of hidden-layer neurons
  //   - (indirectly?) shape of input-layer direct activations (how many)
  //   - (indirectly?) shape of "chromosomes"/genotypes for ...
  // - build by builder
  // - used by something to compute output activations (from input activations
  //   plus a chromosome)






  /* (https://en.wikipedia.org/wiki/Logistic_function;
   * +∞ -> 1
   *  0 -> 0.5
   * -∞ -> 0)
   * "logistic function"? "logistic curve"? generic "activation function"?
   */
  // use logistic curve? tanh?
  def sigmoid(x: Double): Double = 1 / (1 + math.exp(-x))


  private[this] case class Activation(v: Double = 0.5f) extends AnyVal
  private[this] case class Weight(v: Double) extends AnyVal
  private[this] case class Bias(v: Double) extends AnyVal

  type NeuronRef = Int
  private[this] case class Edge1(weight: Weight, sourceRef: NeuronRef)
  private[this] case class Neuron(bias1: Weight, edges: Vector[Edge1])

  private[this] trait Neuron2 {
    def getActivation: Activation
  }
  private[this] trait InputNeuron2 extends Neuron2 {
    def setActivation(activation: Activation): Unit
  }
  private[this] trait NoninputNeuron2 extends Neuron2 {
    def getBias: Bias
    def getInputEdges: Seq[Edge2]
  }
  private[this] trait Edge2 {
    def getSource: Neuron2
    def getWeight: Weight
    def getWeightedActivation: Double  //????
  }

  private[this] class InputNeuron2Impl() extends InputNeuron2 {
    var activation: Activation = _
    override def setActivation(activation: Activation): Unit = {
      this.activation = activation
    }
    override def getActivation: Activation = activation
  }

  private[this] class Edge2Impl(source: Neuron2, weight: Weight) extends Edge2 {
    override def getSource: Neuron2 = source
    override def getWeight: Weight = weight
    override def getWeightedActivation: Double = {
      source.getActivation.v * weight.v
    }
  }

  private[this] class NoninputNeuron2Impl(bias: Bias, inputEdges: Seq[Edge2]) extends NoninputNeuron2 {
    override def getBias: Bias = bias

    override def getInputEdges: Seq[Edge2] = inputEdges

    override def getActivation: Activation = {
      val rawSum =
        getBias.v +
            getInputEdges
                .map(e => e.getWeight.v * e.getSource.getActivation.v)
                .fold(0d)(_ + _)

      val coreResult = sigmoid(rawSum)
      Activation(coreResult)
    }
  }

  private[this] val in1 = new InputNeuron2Impl()
  private[this] val in2 = new InputNeuron2Impl()

  private[this] val e1 = new Edge2Impl(in1, Weight(1))
  private[this] val e2 = new Edge2Impl(in2, Weight(-1))
  private[this] val xn1 = new NoninputNeuron2Impl(Bias(0), Vector(e1, e2))

  in1.setActivation(Activation(0.2))
  in2.setActivation(Activation(0.1))

  println(s"in1.getActivation = ${in1.getActivation}")
  println(s"in2.getActivation = ${in2.getActivation}")
  println(s"xn1.getActivation = ${xn1.getActivation}")
  println("---")

  private[this] val lines = Vector(
    ( (1, 1), (1, 2), (1, 3) ),
    ( (2, 1), (2, 2), (2, 3) ),
    ( (3, 1), (3, 2), (3, 3) ),
    ( (1, 1), (2, 1), (3, 1) ),
    ( (1, 2), (2, 2), (3, 2) ),
    ( (1, 3), (2, 2), (3, 1) ),
    ( (1, 1), (2, 2), (3, 3) ),
    ( (1, 3), (2, 3), (3, 3) ),
    )



  //  trait CellPosition

  private[this] case class CellCoordinates(rowOrdinal: Int,
                                           columnOrdinal: Int) //??extends CellPosition;



/*
  sealed trait CellState { }
  case object Empty extends CellState
  trait Marked extends CellState
  case object MarkedX extends Marked
  case object MarkedO extends Marked

  case class Cell( state: CellState )

  case class BoardRepr1(
                           cells: Tuple9[CellState, CellState, CellState, CellState, CellState, CellState, CellState, CellState, CellState]
                           = (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty));
  case class BoardRepr2(cells:
                        (
                            (CellState, CellState, CellState),
                                (CellState, CellState, CellState),
                                (CellState, CellState, CellState))
                        = ((Empty, Empty, Empty), (Empty, Empty, Empty), (Empty, Empty, Empty)))
  case class CellNumber(ordinal: Int) extends CellPosition;
  val Size = 3
  case class BoardRepr3(cells:  Map[CellPosition, CellState]
                       = 1.to(3 * 3).map(p => CellNumber(p) -> Empty).toMap)



  /*
    Board operations:
    - get state of all cells (in some form(s))
    - mark a(n empty) cell with a valid mark
    Higher level?:
    - report any winner (report any three-in-a-row row(s))



   */


  /*

   */

*/
}
