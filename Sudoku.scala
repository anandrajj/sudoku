import scala.annotation.tailrec

object Sudoku {
  def main(args: Array[String]) {
  val maxRow = 9
  val maxCol = 9
  val gridSize = 9 * 9
  val boxSize = Math.sqrt(maxRow).toInt
  
  //Reads the input file path from the first position of input Args
  val test = for (line <- scala.io.Source.fromFile(args(0)).getLines()) yield line.replaceAll(" ", "").split("|").filter(x => x != "|" ).map(x => if (x == ".") 0.toInt else x.toInt)
  val x = test.flatten.toArray 
  
   
  def checkNotPresent(sudokuGrid: Array[Int], row:Int, col: Int, value: Int) : Boolean = {
      val rowStart = (maxRow * row)                 //Calculate Index where the row starts
      val rowEnd = (maxRow * (row + 1))             //Calcualte Index where Row Ends.
       
      
      val notPresentInRow  = sudokuGrid.slice(rowStart,rowEnd).forall(value != ) //Calculate if the input Value contents with other row values
  
      val colIndices = (0 to (maxCol - 1)).map(_ * maxCol + col)        //Find the column indices to be checked for contention
      val notPresentInCol = colIndices.forall(x => sudokuGrid(x) != value)   //Check if the column contents with input value
      
        
       val boxRowStart = row - (row % boxSize)        //For the current row, column index find start of row
       val boxColStart = col - (col % boxSize)        //Similarly find where the column starts
       
       val boxRowEnd = boxRowStart + (boxSize - 1) //End Row index of the box
       val boxColEnd = boxColStart + (boxSize - 1) //End col index of the box
      
       val notPresentInBox = (for (i <- boxRowStart to boxRowEnd) yield          //For the indices in the box check if the new value is already present
          for (j <- boxColStart to boxColEnd) yield i * maxRow + j).flatten.forall(x => sudokuGrid(x) != value)
          
       notPresentInRow && notPresentInCol && notPresentInBox
      
  }

  //Function to Find the next index
  def findNextIndex(grid: Array[Int]): Int = { val x = grid.zipWithIndex.filter(x => x._1 == 0).map(_._2).toList
         if (x.size == 0) gridSize else x.head
  }
  
   
  
  @tailrec def solve(grid: Array[Int], IndexList: List[(Int, Int)]) : Array[Int] = {
      val index = IndexList.head._1                       //Obtain the current index which has to be processed
                  
      if (index == gridSize) grid                         //End of Grid reached...Return solved grid
      else {
        val rowIndex = index / maxRow                     //Find the row position
        val colIndex = index % maxRow                     //Find the col position of the index
        
        //Function to get the value that fits the current index
        def recAssaignNextValue(i: Int): Int = {
            if (i > maxRow) maxRow + 1
            else if(checkNotPresent(grid, rowIndex, colIndex, i)) i
            else recAssaignNextValue(i+1)
        }
        
        val nextValue = recAssaignNextValue(IndexList.head._2 + 1)
        if(nextValue != maxRow + 1) {
                  val newGrid: Array[Int] = Array(grid.slice(0,index), Array(nextValue), grid.slice(index+1, grid.size)).flatten   //Assign the value i to the Index if all checks passed
                  val newIndexList = (findNextIndex(newGrid), 0) :: (index, nextValue) :: IndexList.tail  //Keep track the changes made in the additional list.
                  solve(newGrid, newIndexList)      //Recursively call the solve
        }
        else if(index == 0 && nextValue == maxRow + 1) grid   //If in the first index, all values are tried. Then break the processing as there is no solution.
        else 
        {
          val unassignIndex = IndexList.tail.head._1
          val unassignedGrid = Array(grid.slice(0,unassignIndex), Array(0), grid.slice(unassignIndex+1, grid.size)).flatten //un-assign the current value 
          solve(unassignedGrid, IndexList.tail)   //BackTrack to previous position 
        }
      }
  }
  
  val nextIndex = findNextIndex(x)
  println("Input grid - " + x.mkString(","))
  println("Final Solution - " + solve(x, List((nextIndex, x(nextIndex)))).mkString(",")) 
  }
}
