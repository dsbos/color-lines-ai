package com.us.dsb.explore.algs.ttt


object ComputeLineRows extends App {

  //  trait CellPosition
  case class CellCoordinates(rowOrdinal: Int,
                             columnOrdinal: Int) //??????extends CellPosition;

  val nameThis = {
    import scala.collection.{SortedSet, mutable}
    implicit def orderingForCellCoordinates[A <: CellCoordinates]: Ordering[A] =
      Ordering.by(cc => (cc.rowOrdinal, cc.columnOrdinal))
    implicit def orderingByNameThis[A <: SortedSet[CellCoordinates]]: Ordering[A] =
      Ordering.by(row => row.toString())

    SortedSet[CellCoordinates]()
    mutable.SortedSet[SortedSet[CellCoordinates]]()
    val crudeSet = mutable.SortedSet[SortedSet[CellCoordinates]]()
    for (row1 <- 1 to 3;
         col1 <- 1 to 3;
         //_ <- { println( s"row1: $row1, col1: $col1" ); Some(null) };
         row2 <- 1 to 3;
         col2 <- 1 to 3;
         //_ <- { println( s"row1: $row1, col1: $col1; row2: $row2, col2: $col2" ); Some(null) };
         row3 <- 1 to 3;
         col3 <- 1 to 3//;
         //_ <- { println( s"row1: $row1, col1: $col1; row2: $row2, col2: $col2; row3: $row3, col3: $col3" ); Some(null) }
         ) {
      //println()
      //println( s"row1: $row1, col1: $col1; row2: $row2, col2: $col2; row3: $row3, col3: $col3:" );
      val rowSomething = row1 - row2 == row2 - row3
      val colSomething = col1 - col2 == col2 - col3
      val rowSomethingMore = row1 - row2 <= 0
      val sameRow = row1 == row2 && row2 == row3
      val sameCol = col1 == col2 && col2 == col3
      //println( s"$row1/$col1, $row2/$col2, $row3/$col3: rowSomething = $rowSomething" );
      //println( s"$row1/$col1, $row2/$col2, $row3/$col3: sameRow = $sameRow" );
      //println( s"$row1/$col1, $row2/$col2, $row3/$col3: colSomething = $colSomething" );
      //println( s"$row1/$col1, $row2/$col2, $row3/$col3: sameCol = $sameCol" );
      val makeARow = (rowSomething && colSomething)  && ! (sameRow && sameCol) && rowSomethingMore
      //println( s"$row1/$col1, $row2/$col2, $row3/$col3: makeARow = $makeARow" );
      if (makeARow) {
        //println( s"$row1/$col1, $row2/$col2, $row3/$col3: $makeARow ************" );
        val cell1Id = CellCoordinates(row1, col1)
        val cell2Id = CellCoordinates(row2, col2)
        val cell3Id = CellCoordinates(row3, col3)

        val row = SortedSet(cell1Id, cell2Id, cell3Id)
        // println(s"row = ${row}")
        crudeSet.add(row)
      }

    }
    //println(s"crudeSet = $crudeSet")

    println("crudeSet")
    for (rowSet <- crudeSet) {
      print("- ")
      for (CellCoordinates(row, col) <- rowSet) {
        print(s"$row/$col ")
      }
      println()
    }
    //crudeSet.zipWithIndex.map(x => ???);
    //crudeSet.zipWithIndex.map((x) => ???);
    //crudeSet.zipWithIndex.map((x1, x2) => ???);
    //crudeSet.zipWithIndex.map(pair => ???)
    crudeSet.zipWithIndex.map({ case (lineCoords, lineOffset) =>
      (lineOffset + 1,
          (for (line <- lineCoords) yield {
            (line.rowOrdinal, line.columnOrdinal)
          }).toSeq
      )
    }).toVector
    //crudeSet.zipWithIndex.map( case (rowLine, offset) =>  ??? );
  }
  println(s"nameThis: ${nameThis.sortBy(_._1).mkString("\n- ", "\n- ", "")}")


}
