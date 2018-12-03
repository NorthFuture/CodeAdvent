package Year2016

import scala.io.Source

object Day6 extends App {

  val input = Source.fromFile("data/Day6.txt").getLines().toList

  val result = input.map(l => l.toList).transpose.map(_.groupBy(identity).maxBy(_._2.length)._1)
  val result2= input.map(l => l.toList).transpose.map(_.groupBy(identity).minBy(_._2.length)._1)

  System.out.println(result.mkString(""))
  System.out.println(result2.mkString(""))

}
