package Year2018

import scala.annotation.tailrec
import scala.collection.mutable

object Day11 extends App {

  type Cell = (Int, Int)

  val gridSerialNumber = 9445

  val TotalSize = 300

  def powerCell(x: Int, y: Int) = {
    val rackId = x + 10

    ((rackId * y + gridSerialNumber) * rackId % 1000 / 100) - 5
  }

  val powerCells = mutable.ArrayBuffer.fill(TotalSize, TotalSize)(0)

  for {
    x <- 0 to TotalSize - 1
    y <- 0 to TotalSize - 1
  } yield powerCells(x)(y) = powerCell(x, y)

  def findBestCellGrid(gridSize: Int): (Cell, Int) = {

    val origins = for {
      origin_x <- 0 to TotalSize - gridSize - 1
      origin_y <- 0 to TotalSize - gridSize - 1
    } yield (origin_x, origin_y)

    origins.map { origin => calculateGridPower(origin, 0, origin._1, origin._2, gridSize, gridSize) }.maxBy(_._2)
  }

  @tailrec
  def calculateGridPower(originCell: Cell, totalPower: Int, currentX: Int, currentY: Int, widthX: Int, widthY: Int): (Cell, Int) = {

    if (currentY == originCell._2 + widthY) {
      (originCell, totalPower)
    } else if (currentX == originCell._1 + widthX) {
      calculateGridPower(originCell, totalPower, originCell._1, currentY + 1, widthX, widthY)
    } else {
      calculateGridPower(originCell, powerCells(currentX)(currentY) + totalPower, currentX + 1, currentY, widthX, widthY)
    }
  }

  val result1 = findBestCellGrid(3)

  println(result1)

  @tailrec
  def search(max: (Cell, Int), width: Seq[Int]): (Cell, Int) = {

    width match {
      case h :: tail =>
        val res = findBestCellGrid(h)

        if (res._2 > max._2) {
          println(res + "-" + h)
          search(res, tail)
        } else {
          search(max, tail)
        }
      case Nil => max
    }
  }

  val result2 = search(((-1, -1), Int.MinValue), (0 to 299).toList)

  println(result2)
}

