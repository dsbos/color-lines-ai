package ttt

import scala.tools.nsc.doc.html.HtmlTags.Input


object TicTacToeSomething extends App {


  trait NeuralStructure
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


  case class Activation(v: Double = 0.5f) extends AnyVal
  case class Weight(v: Double) extends AnyVal
  case class Bias(v: Double) extends AnyVal

  type NeuronRef = Int
  case class Edge1(weight: Weight, sourceRef: NeuronRef)
  case class Neuron(bias1: Weight, edges: Vector[Edge1])

  trait Neuron2 {
    def getActivation: Activation
  }
  trait InputNeuron2 extends Neuron2 {
    def setActivation(activation: Activation): Unit
  }
  trait NoninputNeuron2 extends Neuron2 {
    def getBias: Bias
    def getInputEdges: Seq[Edge2]
  }
  trait Edge2 {
    def getSource: Neuron2
    def getWeight: Weight
    def getWeightedActivation: Double  //????
  }

  class InputNeuron2Impl() extends InputNeuron2 {
    var activation: Activation = _
    override def setActivation(activation: Activation): Unit = {
      this.activation = activation
    }
    override def getActivation: Activation = activation
  }

  class Edge2Impl(source: Neuron2, weight: Weight) extends Edge2 {
    override def getSource: Neuron2 = source
    override def getWeight: Weight = weight
    override def getWeightedActivation: Double = {
      source.getActivation.v * weight.v
    }
  }

  class NoninputNeuron2Impl(bias: Bias, inputEdges: Seq[Edge2]) extends NoninputNeuron2 {
    override def getBias: Bias = bias

    override def getInputEdges: Seq[Edge2] = inputEdges

    override def getActivation: Activation = {
      val rawSum =
        getBias.v +
            getInputEdges
                .map(e => e.getWeight.v * e.getSource.getActivation.v)
                .fold(0d)(_ + _)

      val coreResult = sigmoid(rawSum)
      return Activation(coreResult)
    }
  }

  val in1 = new InputNeuron2Impl()
  val in2 = new InputNeuron2Impl()

  val e1 = new Edge2Impl(in1, Weight(1))
  val e2 = new Edge2Impl(in2, Weight(-1))
  val xn1 = new NoninputNeuron2Impl(Bias(0), Vector(e1, e2))

  in1.setActivation(Activation(0.2))
  in2.setActivation(Activation(0.1))

  println(s"in1.getActivation = ${in1.getActivation}")
  println(s"in2.getActivation = ${in2.getActivation}")
  println(s"xn1.getActivation = ${xn1.getActivation}")
  println("---")

  val lines = Vector(
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

  case class CellCoordinates(rowOrdinal: Int,
                             columnOrdinal: Int) //??????extends CellPosition;



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
