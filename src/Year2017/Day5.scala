package Year2017

import scala.annotation.tailrec
import scala.io.Source

object Day5 extends App {

  val steps = Source.fromFile("data/Year2017/Day5.txt").getLines().zipWithIndex.map(x => x._2 -> x._1.toInt).toMap

  @tailrec
  def processInstruction(currentInstruction: Int, steps: Map[Int, Int], count: Int, offsetProcesser: Int => Int): Int = {
    steps.lift
    steps.get(currentInstruction) match {
      case None => count
      case Some(x) =>
        val newSteps = steps.+((currentInstruction, offsetProcesser(x)))
        processInstruction(currentInstruction + x, newSteps, count + 1, offsetProcesser)
    }
  }

  println(processInstruction(0, steps, 0, x => x + 1))
  println(processInstruction(0, steps, 0, x => if (x >= 3) x - 1 else x + 1))
}
