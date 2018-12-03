package Year2017

import scala.io.Source

object Day11 extends App with Benchmark {

  def distance(pos: (Int, Int)): Int = {
    if (math.abs(pos._1) > math.abs(pos._2)) {
      math.abs(pos._1)
    } else {
      (math.abs(pos._2) - math.abs(pos._1)) / 2 + math.abs(pos._1)
    }
  }

  Source.fromFile("data/Year2017/Day11.txt").getLines()
    .map(_.split(",")
      .scanLeft((0, 0))((s, d) =>
        d match {
          case "n" => (s._1, s._2 + 2)
          case "s" => (s._1, s._2 - 2)
          case "nw" => (s._1 - 1, s._2 + 1)
          case "sw" => (s._1 - 1, s._2 - 1)
          case "ne" => (s._1 + 1, s._2 + 1)
          case "se" => (s._1 + 1, s._2 - 1)
        })
    ).map(x => (distance(x.last), distance(x.maxBy(distance)))
  ).foreach(println)


}

