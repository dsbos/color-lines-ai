package com.us.dsb.explore.algs.coloredlines

//import scala.tools.nsc.doc.html.HtmlTags.Input


private object XxxTicTacToeSomething extends App {


  private trait XxNeuralStructure
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


  private case class XxActivation(v: Double = 0.5f) extends AnyVal
  private case class XxWeight(v: Double) extends AnyVal
  private case class XxBias(v: Double) extends AnyVal

  type XxNeuronRef = Int
  private case class XxEdge1(weight: XxWeight, sourceRef: XxNeuronRef)
  private case class XxNeuron(bias1: XxWeight, edges: Vector[XxEdge1])

  private trait XxNeuron2 {
    def getActivation: XxActivation
  }
  private trait XxInputNeuron2 extends XxNeuron2 {
    def setActivation(activation: XxActivation): Unit
  }
  private trait XxNoninputNeuron2 extends XxNeuron2 {
    def getBias: XxBias
    def getInputEdges: Seq[XxEdge2]
  }
  private trait XxEdge2 {
    def getSource: XxNeuron2
    def getWeight: XxWeight
    def getWeightedActivation: Double  //????
  }

  private class XxInputNeuron2Impl() extends XxInputNeuron2 {
    var activation: XxActivation = _
    override def setActivation(activation: XxActivation): Unit = {
      this.activation = activation
    }
    override def getActivation: XxActivation = activation
  }

  private class XxEdge2Impl(source: XxNeuron2, weight: XxWeight) extends XxEdge2 {
    override def getSource: XxNeuron2 = source
    override def getWeight: XxWeight = weight
    override def getWeightedActivation: Double = {
      source.getActivation.v * weight.v
    }
  }

  private class XxNoninputNeuron2Impl(bias: XxBias, inputEdges: Seq[XxEdge2]) extends XxNoninputNeuron2 {
    override def getBias: XxBias = bias

    override def getInputEdges: Seq[XxEdge2] = inputEdges

    override def getActivation: XxActivation = {
      val rawSum =
        getBias.v +
            getInputEdges
                .map(e => e.getWeight.v * e.getSource.getActivation.v)
                .fold(0d)(_ + _)

      val coreResult = sigmoid(rawSum)
      XxActivation(coreResult)
    }
  }

  private val in1 = new XxInputNeuron2Impl()
  private val in2 = new XxInputNeuron2Impl()

  private val e1 = new XxEdge2Impl(in1, XxWeight(1))
  private val e2 = new XxEdge2Impl(in2, XxWeight(-1))
  private val xn1 = new XxNoninputNeuron2Impl(XxBias(0), Vector(e1, e2))

  in1.setActivation(XxActivation(0.2))
  in2.setActivation(XxActivation(0.1))

  println(s"in1.getActivation = ${in1.getActivation}")
  println(s"in2.getActivation = ${in2.getActivation}")
  println(s"xn1.getActivation = ${xn1.getActivation}")
  println("---")

  private val lines = Vector(
    ( (1, 1), (1, 2), (1, 3) ),
    ( (2, 1), (2, 2), (2, 3) ),
    ( (3, 1), (3, 2), (3, 3) ),
    ( (1, 1), (2, 1), (3, 1) ),
    ( (1, 2), (2, 2), (3, 2) ),
    ( (1, 3), (2, 2), (3, 1) ),
    ( (1, 1), (2, 2), (3, 3) ),
    ( (1, 3), (2, 3), (3, 3) ),
    )



  //  trait XxCellPosition

  private case class XxCellCoordinates(rowOrdinal: Int,
                             columnOrdinal: Int) //??????extends CellPosition;



/*
  sealed trait XxCellState { }
  case object XxEmpty extends CellState
  trait XxMarked extends CellState
  case object XxMarkedX extends Marked
  case object XxMarkedO extends Marked

  case class XxCell( state: CellState )

  case class XxBoardRepr1(
                           cells: Tuple9[CellState, CellState, CellState, CellState, CellState, CellState, CellState, CellState, CellState]
                           = (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty));
  case class XxBoardRepr2(cells:
                        (
                            (CellState, CellState, CellState),
                                (CellState, CellState, CellState),
                                (CellState, CellState, CellState))
                        = ((Empty, Empty, Empty), (Empty, Empty, Empty), (Empty, Empty, Empty)))
  case class XxCellNumber(ordinal: Int) extends CellPosition;
  val Size = 3
  case class XxBoardRepr3(cells:  Map[CellPosition, CellState]
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