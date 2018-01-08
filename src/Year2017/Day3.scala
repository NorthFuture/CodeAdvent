package Year2017

import scala.reflect.ClassTag

object Day3 extends App {

  def numbersCountInSpiral(spiralNo: Int) = if (spiralNo == 1) {
    1
  } else {
    val n = spiralNo * 2 - 1
    n * 2 + (n - 2) * 2
  }

  def lastNumberInSpiral(spiralNo: Int) = if (spiralNo == 1) {
    1
  }
  else {
    (spiralNo * 2 - 1) * (spiralNo * 2 - 1)
  }

  def findDistance(number: Int): Int = {

    val r = matrix.find(number)
    if (r.isEmpty) {
      -1
    } else {
      math.abs(r.head._1) + math.abs(r.head._2)
    }

  }

  val SIZE = 1700

  // MUTABLE CLASS
  class CenteredMatrix[T](size: Int, defaultValue: T)(implicit evidence$10: scala.reflect.ClassTag[T]) {
    val array: Array[Array[T]] = Array.fill(size, size)(defaultValue)

    def update(r: Int, c: Int, value: T) = setCell(r, c, value)

    def apply(r: Int, c: Int) = getCell(r, c)

    def apply(pos: (Int, Int)) = getCell(pos._1, pos._2)

    def getCell(r: Int, c: Int): T = {
      array(-r + size / 2)(c + size / 2)
    }

    def setCell(r: Int, c: Int, value: T) = {
      array(-r + size / 2)(c + size / 2) = value
    }

    def print(printWidth: Int, format: String) = {
      val r = array.drop(size / 2 - printWidth).take(printWidth * 2).map(_.drop(size / 2 - printWidth).take(printWidth * 2).map(_.formatted(format)).mkString(" ")).mkString("\n")

      println(r)
    }

    def find(value: T): Seq[(Int, Int)] = {
      for {
        r <- matrix.array.zipWithIndex
        c <- r._1.zipWithIndex
        if (c._1 == value)
      } yield (r._2 - size / 2, c._2 - size / 2)
    }
  }

  val matrix = (1 to 300).foldLeft(new CenteredMatrix(SIZE, 0))((m, spiralNo) => {

    if (spiralNo == 1) {
      m.setCell(0, 0, 1)
    } else {

      val thisNumbersInSpiralCount = numbersCountInSpiral(spiralNo)

      val seed = lastNumberInSpiral(spiralNo - 1)

      1 to (spiralNo * 2 - 2) foreach (x => m.setCell(x - (spiralNo - 1), spiralNo - 1, seed + x))
      1 to (spiralNo * 2 - 2) foreach (x => m.setCell(spiralNo - 1, -x + (spiralNo - 1), x + seed + spiralNo * 2 - 2))
      1 to (spiralNo * 2 - 2) foreach (x => m.setCell(-x + (spiralNo - 1), -(spiralNo - 1), x + seed + (spiralNo * 2 - 2) * 2))
      1 to (spiralNo * 2 - 2) foreach (x => m.setCell(-(spiralNo - 1), x - (spiralNo - 1), x + seed + (spiralNo * 2 - 2) * 3))
    }
    m
  })

  val matrix2 = (1 to 400).foldLeft(new CenteredMatrix(SIZE, 0))((m, spiralNo) => {

    def computeSum(r: Int, c: Int): Int = {
      m.getCell(r - 1, c - 1) + m.getCell(r - 1, c) + m.getCell(r - 1, c + 1) +
        m.getCell(r, c - 1) + m.getCell(r, c) + m.getCell(r, c + 1) +
        m.getCell(r + 1, c - 1) + m.getCell(r + 1, c) + m.getCell(r + 1, c + 1)
    }

    def updateMatrix(r: Int, c: Int) = {

      val value = if (r == 0 && c == 0) {
        1
      } else {
        computeSum(r, c)
      }

      m.setCell(r, c, value)
    }

    if (spiralNo == 1) {
      updateMatrix(0, 0)
    } else {

      val thisNumbersInSpiralCount = numbersCountInSpiral(spiralNo)

      val seed = lastNumberInSpiral(spiralNo - 1)

      1 to (spiralNo * 2 - 2) foreach (x => updateMatrix(x - (spiralNo - 1), spiralNo - 1))
      1 to (spiralNo * 2 - 2) foreach (x => updateMatrix(spiralNo - 1, -x + (spiralNo - 1)))
      1 to (spiralNo * 2 - 2) foreach (x => updateMatrix(-x + (spiralNo - 1), -(spiralNo - 1)))
      1 to (spiralNo * 2 - 2) foreach (x => updateMatrix(-(spiralNo - 1), x - (spiralNo - 1)))
    }
    m
  })

  //matrix.print(10, "%4d")
  matrix2.print(5, "%10d")

  val r = for {
    r <- matrix.array.zipWithIndex
    c <- r._1.zipWithIndex
    if (c._2 > 312051)
  } yield c._2

  println(r.head)

  println(findDistance(312051))

}
