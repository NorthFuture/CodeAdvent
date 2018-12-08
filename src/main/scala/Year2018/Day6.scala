package Year2018

import scala.annotation.tailrec
import scala.io.Source

object Day6 extends App {

  val reg ="""(\d+), (\d+)""".r

  val input = Source.fromFile("data/Year2018/Day6.txt").getLines().map {
    case reg(x, y) => (x.toInt, y.toInt)
  }.toList

  input.foreach(println)

}
