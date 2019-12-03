package Year2019

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day1 extends App {

  def getInput(): Seq[Int] =
    Source.fromFile("data/Year2019/Day1.txt").getLines().map(_.toInt).toSeq

  def calculateFuelRequirment(x: Int) = x / 3 - 2

  def solvePart1(input: Seq[Int]) =
    input.map(calculateFuelRequirment).foldLeft(0)(_ + _)

  @tailrec
  def solvePart2(input: Seq[Int], result: Int): Int = {
    input.map(calculateFuelRequirment).filter(_ > 0) match {
      case Nil => result
      case x   => solvePart2(x, result + x.foldLeft(0)(_ + _))
    }
  }

  println(solvePart1(getInput))

  println(solvePart2(getInput, 0))

}
