package Year2017

import scala.io.Source

object Day4 extends App {

  val result1 = Source.fromFile("data/Year2017/Day4.txt").getLines().map(_.split("\\s").groupBy(x => x))
    .filter(_.filter(_._2.size > 1).size == 0)

  val result2 = Source.fromFile("data/Year2017/Day4.txt").getLines().map(_.split("\\s").map(x => x.toSeq.sortBy(x => x).mkString("")))
    .map(x => x.groupBy(x => x))
    .filter(_.filter(_._2.size > 1).size == 0)

  println(result1.size)
  println(result2.size)

}
