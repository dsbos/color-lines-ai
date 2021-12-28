package com.us.dsb.explore.swing

import java.awt.geom.Rectangle2D
import java.awt.{Color, Dimension}
import scala.swing.BorderPanel.Position
import scala.swing._


private object ElevatorAnimationExplApp extends App {

  class SimpleElevatorPanel extends Panel {

    // Static parameters:
    private val CarHeight = 20
    private val CarWidth = 25

    private val HoistwayHeight = 300
    

    private val HorizontalSpace = 3
    private val VerticalSpace = 3

    private val HoistwayWidth = CarWidth + 2 * HorizontalSpace

    private val CarMinY = 0 + VerticalSpace
    private val CarMaxY = HoistwayHeight - CarHeight - VerticalSpace

    private val CarYPerTick = 0.1
    private val TickMs = 1

    private var HoistwayRect = new Rectangle2D.Float(HorizontalSpace.toFloat, 0, HoistwayWidth.toFloat, HoistwayHeight.toFloat)

    private object MovementState extends Enumeration {
      type MovementState = Value
      val Stopped, Ascending, Descending = Value
    }

    // State variables
    private var carState: MovementState.Value = MovementState.Stopped
    private var carY: Double = CarMinY

    private val timer = new javax.swing.Timer(TickMs, null)


    private def carRect = new Rectangle2D.Double(HorizontalSpace, carY, CarWidth, CarHeight)

    def startUp(): Unit = {
      carState = MovementState.Ascending
      timer.start()
    }

    def startDown(): Unit = {
      carState = MovementState.Descending
      timer.start()
    }



    private def doWithNarrowedRepainting(f: => Unit): Unit = {
      val prevRectViewBounds = carRect.getBounds
      f
      val curRectViewBounds = carRect.getBounds
      repaint(prevRectViewBounds)
      repaint(curRectViewBounds)
    }

    timer.addActionListener(Swing.ActionListener(event => {
      val (nextCarState, nextCarY) = {
        var curState = carState
        var curY = carY
        import MovementState._
        curState match {
          case Stopped =>
            timer.stop()
          case Ascending =>
            curY = (curY - CarYPerTick) max CarMinY
            if (curY == CarMinY) {
              curState = Stopped
            }
          case Descending =>
            curY = (curY + CarYPerTick) min CarMaxY
            if (curY == CarMaxY) {
              curState = Stopped
            }
        }
        (curState, curY)
      }


      doWithNarrowedRepainting({
        carState = nextCarState
        carY = nextCarY
      })
    }) )

    override def paint(g: Graphics2D): Unit = {
      g.setColor(Color.LIGHT_GRAY)
      g.fill(HoistwayRect)


      g.setColor(Color.GREEN)
      val boundsRect = carRect.getBounds
      g.fillRect(boundsRect.x, boundsRect.y, boundsRect.width, boundsRect.height)

    }
    preferredSize = new Dimension(200, HoistwayHeight + 2 * VerticalSpace)

  }


  private val elevatorPanel = new SimpleElevatorPanel {

    override def repaint(rect: Rectangle): Unit = {
      //println(s"Panel repaint($rect)")
      super.repaint(rect)
    }
    preferredSize = new Dimension(150, 300)

  }

  private val window = new MainFrame {
    //size = new Dimension(300, 300)
    title = "Window Title"//
    contents = new BorderPanel {//
      layout += Button("Ascend")(elevatorPanel.startUp()) -> Position.North
      layout += elevatorPanel -> Position.Center//
      layout += Button("Descend")(elevatorPanel.startDown()) -> Position.South
    }//

    //size = new Dimension(200, 400)//

  }//
  window.visible = true//


}
