package com.us.dsb.explore.libs.swing

import java.awt.{BasicStroke, Color, Dimension}
import java.awt.geom.{AffineTransform, Ellipse2D, Rectangle2D}
import scala.swing.BorderPanel.Position
import scala.swing._

// re" reflective access of structural type member variable blockRot should be enabled" ...
import scala.language.reflectiveCalls



object DrawExplApp extends App {
  println("main started...")


  val drawPanel = new Panel {

    var blockShape = new Rectangle2D.Float(10, 10, 20, 20)
    var blockRot = AffineTransform.getRotateInstance(math.Pi / 16)


    var xxx = 10
    var yxx = 10
    var treeDepth = 2

    override def repaint(rect: Rectangle): Unit = {
      println(s"Panel repaint($rect)")
      super.repaint(rect)
    }

    override def paint(g: Graphics2D): Unit = {
      println("Panel paint; size = " + size)
      println("Panel paint; bounds = " + bounds)
      println("Panel paint; g.getClipBounds() = " + g.getClipBounds())
      println("Panel paint; g.getTransform() = " + g.getTransform)
      g.transform(blockRot)

      var blockView = g.getTransform.createTransformedShape(blockShape)

//g.getTransform.createTransformedShape()

      if (true) {
        var count = 0
        println("Doing ...")
        (0 to 255 by 10).reverse.foreach(l => {
          Thread.sleep(1)
          g.setColor(new Color(l, l, l))
          (0 until size.width by 2).foreach(x => {
            //println("x = " + x)
            if (x >= g.getClipBounds.x &&
                x <= g.getClipBounds.x + g.getClipBounds.width) {
              (0 until size.height by 2).foreach(y => {
                if (y >= g.getClipBounds.y &&
                    y <= g.getClipBounds.y + g.getClipBounds.height) {
                  //println(s"x = $x, y = $y")
                  g.fillRect(x, y, 1, 1)
                  count += 1
                }
              })
            }
          })
        })
        println(s"... done (count = $count)")
      }

      println("blockShape = " + blockShape)

      g.setColor(Color.RED)
      g.draw(blockShape)
      g.setColor(Color.GREEN)
      val boundsRect = blockShape.getBounds
      g.fillRect(boundsRect.x, boundsRect.y, boundsRect.width, boundsRect.height)
      g.setColor(Color.BLUE)
      g.drawLine(boundsRect.x, boundsRect.y, boundsRect.x + boundsRect.width - 1, boundsRect.y + boundsRect.height - 1)




      def drawManualGradientTriangle(): Unit = {
        (0 to size.width by 1).foreach(toX => {
          val x: Float = 1 - 1.0f * toX / size.width
          g.setColor(new Color(x, 0.9f, 0.9f))

          g.drawLine(0, 0, toX, size.height)
        })
      }
      //drawManualGradientTriangle()



      val leafTrim = 20
      def drawAngledTree(levels: Int, rect: Rectangle): Unit = {
        //println(s"drawAngledTree($levels, $rect)")
        levels match {
          case 0 => g.setColor(Color.green)
                    g.drawLine(rect.x,              rect.y,
                               rect.x - leafTrim + rect.width, rect.y - leafTrim + rect.height)
                    g.drawLine(rect.x,              rect.y - leafTrim + rect.height,
                               rect.x - leafTrim + rect.width, rect.y)
          case n if n > 0 =>
            g.setColor(Color.blue)
            val topHeight = rect.height / (1 + levels)
            val bottomHeight = rect.height - topHeight

            val leftWidth  = rect.width / 2 // ??? (1 + levels)
            val rightWidth = rect.width - leftWidth

            g.drawLine(rect.x,                 rect.y + 0,
                       rect.x,                 rect.y - 0 + topHeight)
            g.drawLine(rect.x + 0,             rect.y + 0,
                       rect.x - 0 + leftWidth, rect.y - 0 + topHeight)


            drawAngledTree(levels - 1, new Rectangle(rect.x,             rect.y + topHeight,
                                                     leftWidth,                   bottomHeight))

            drawAngledTree(levels - 1, new Rectangle(rect.x + leftWidth, rect.y + topHeight,
                                                     rightWidth,                  bottomHeight))


          //case _ => ???
        }
      }
      val rect2 = this.bounds
      //val rect = new Rectangle(0, 0, size.width - 0, size.height - 0)
      //g.setColor(Color.BLACK)
      if (false) drawAngledTree(treeDepth, rect2)


      if (false) {
        g.setPaint(Color.blue)
        g.fillRect(xxx, yxx, 100, 100)
        (0 to 10).foreach(n => {
          g.setPaint(new Color(11 + 20 * n, 127 + 7 * n, 127 + 9 * n, 0 + 25 * n))
          g.fill(new Rectangle2D.Double(xxx + 10 * n, yxx + 12 * n, 100 - 5 * n, 100 + 5 * n))
        })
      }

    }
    preferredSize = new Dimension(400, 400)
  }

  val window = new MainFrame {
    //size = new Dimension(300, 300)
    title = "Window Title"
    def nameThis(f: => Unit): Unit = {
      val prevRectView = drawPanel.blockRot.createTransformedShape(drawPanel.blockShape)
      val prevRectViewBounds = prevRectView.getBounds
      f
      val curRectView = drawPanel.blockRot.createTransformedShape(drawPanel.blockShape)
      val curRectViewBounds = curRectView.getBounds
      drawPanel.repaint(prevRectViewBounds)
      drawPanel.repaint(curRectViewBounds)
    }
    contents = new BorderPanel {
      layout += Button("ButtonN")({nameThis({drawPanel.blockShape.y -= 6})}) -> Position.North
      layout += Button("ButtonW")({nameThis({drawPanel.blockShape.x -= 6})}) -> Position.West
      layout += drawPanel -> Position.Center
      layout += Button("ButtonE")({nameThis({drawPanel.blockShape.x += 6})}) -> Position.East
      layout += Button("ButtonS")({nameThis({drawPanel.blockShape.y += 6})}) -> Position.South
    }

    //size = new Dimension(200, 400)

  }
  window.visible = true

  println("...main ending")

}
