package Year2018

import scala.annotation.tailrec
import scala.io.Source

object Day6 extends App {

  val reg ="""(\d+), (\d+)""".r

  implicit class PairExtension(val a: Point) extends AnyVal {

    implicit def -(b: Point): Int = {
      Math.abs(a.x - b.x) + Math.abs(a.y - b.y)
    }

    implicit def >(b: Point): Boolean = {
      a.x > b.x && a.y > b.y
    }

    implicit def <(b: Point): Boolean = {
      a.x < b.x && a.y < b.y
    }
  }

  case class Point(x: Int, y: Int)

  case class Area(id: Char, origin: Point)

  val areas = Source.fromFile("data/Year2018/Day6.txt").getLines().map {
    case reg(x, y) => Point(x.toInt, y.toInt)
  }.zipWithIndex.map(x => Area(('a' + x._2).toChar, x._1)).toList

  // TODO: for 1 scan replace with foldLeft/tailrect
  val min = Point(areas.map(_.origin.x).min, areas.map(_.origin.y).min)
  val max = Point(areas.map(_.origin.x).max, areas.map(_.origin.y).max)

  val coords = for {
    x <- min.x to max.x
    y <- min.y to max.y
  } yield Point(x, y)

  val searchGridWithArea = coords.flatMap(c => {
    val dists = areas.map(x => (x, x.origin - c)).sortBy(_._2)

    if (dists.length == 0 || (dists.length > 1 && dists(0)._2 == dists(1)._2)) {
      None
    } else {
      Some(c, dists(0)._1)
    }
  }) toMap

  def printArea() = {
    for (y <- min.y to max.y + 10) {
      for (x <- min.x to max.x + 10) {
        searchGridWithArea.get(Point(x, y)) match {
          case None => print(" ")
          case Some(a) =>
            print(if (a.origin == Point(x, y)) a.id.toUpper else a.id.toLower)
        }
      }
      println()
    }
  }

  //printArea()

  val rr = searchGridWithArea
    .groupBy(x => x._2)
    .filter(x => x._2.forall(y => y._1 > min && y._1 < max))

  println(rr.maxBy(_._2.size)._2.size)

  //3840
}

