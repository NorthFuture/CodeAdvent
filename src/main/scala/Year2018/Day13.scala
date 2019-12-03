package Year2018


import scala.io.Source

object Day13 extends App {

  val map = Source.fromFile("data/Year2018/Day13.txt").getLines().map(x => x.toList).toList

  val r=map.map(_.mkString("")).mkString("\n")


}

