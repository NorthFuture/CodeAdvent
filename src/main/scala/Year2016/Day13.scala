package Year2016

import scala.annotation.tailrec

object Day13 extends App {

  val input = 1364L

  val size = 80

  def valueAtXY(x: Int, y: Int): Long = {
    input + x * x + 3 * x + 2 * x * y + y + y * y
  }

  def numBitsSet(x: Long): Long = {
    x.toBinaryString.toList.count(_ != '0')
  }

  def isWall(x: Int, y: Int): Boolean = {
    numBitsSet(valueAtXY(x, y)) % 2 != 0
  }

  val matrix = 0 to size map { x =>
    0 to size map { y =>
      isWall(y, x)
    }
  }


  @tailrec
  def solve[S](currentStates: Seq[Seq[S]], isStateValid: (S) => Boolean, isGoalState: (S) => Boolean, calculateNewStates: (S) => Seq[S]): Seq[Seq[S]] = {

    val newStates = for {
      s <- currentStates
      ns <- calculateNewStates(s.last) if (isStateValid(ns) && !s.exists(_ == ns))
    } yield (s :+ ns)

    if (newStates.isEmpty) {
      newStates
    } else {

      val solution = newStates.filter(s => isGoalState(s.last))

      if (!solution.isEmpty) {
        solution
      } else {
        solve(newStates, isStateValid, isGoalState, calculateNewStates)
      }
    }
  }

  type State = (Int, Int)

  def isStateValid(s: State): Boolean = {
    !isWall(s._1, s._2) && s._1 >= 0 && s._2 > 0 && s._1 <= size && s._2 <= size
  }

  def isGoalState(s: State): Boolean = {
    s._1 == 31 && s._2 == 39
  }

  def calculateNewStates(s: State): Seq[State] = {
    Seq((s._1 - 1, s._2), (s._1 + 1, s._2), (s._1, s._2 - 1), (s._1, s._2 + 1))
  }

  val r: Seq[Seq[(Int, Int)]] = solve(Seq(Seq((1, 1))), isStateValid, isGoalState, calculateNewStates)

  println(r.head.take(50).distinct.size)

  def printMatrix(x: Seq[Seq[Boolean]]): String = {
    "   " + (0 to x.length).map(_ % size).map(_.toString).mkString(" ") + "\n" +
      x.zipWithIndex.map(y => y._2.formatted("%02d") + " " + y._1.map(if (_) '#' else '.').mkString(" ")).mkString("\n")
  }

 // println(printMatrix(matrix))

}

