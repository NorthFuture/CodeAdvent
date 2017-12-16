package Year2017

import scala.annotation.tailrec
import scala.io.Source

object Day1_2 extends App {

  Source.fromFile("data/Year2017/Day1.txt").getLines().map(s => s.splitAt(s.size / 2))
    .map(x => x._1.toSeq.zip(x._2.toSeq))
    .map(_.filter(x => x._1 == x._2))
    .map(x => x.map(x => x._1 - '0' + x._2 - '0').sum)
    .foreach(println)
}
