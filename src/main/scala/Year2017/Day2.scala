package Year2017

import scala.io.Source

object Day2 extends App {

  val result1 = Source.fromFile("data/Year2017/Day2.txt").getLines().map(_.split(" |\t"))
    .map(s => s.map(_.toInt).foldLeft((Integer.MAX_VALUE, Integer.MIN_VALUE))((s, x) => (if (x < s._1) x else s._1, if (x > s._2) x else s._2)))
    .map(x => x._2 - x._1)
    .sum

  val result2 = Source.fromFile("data/Year2017/Day2.txt").getLines().map(_.split(" |\t"))
    .map(s => s.map(_.toInt)
      .combinations(2)
      .map(x => if (x.head % x.last == 0) x.head / x.last else if (x.last % x.head == 0) x.last / x.head else 0).sum
    ).sum

  println(result1)
  println(result2)
}
