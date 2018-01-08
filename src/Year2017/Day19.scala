package Year2017

import scala.annotation.tailrec
import scala.io.Source

object Day19 extends App with Benchmark {

  val map = Source.fromFile("data/Year2017/Day19.txt").getLines()
    .map(x => x.toCharArray.toList).toList

  val startingPoint = (0, map(0).indexOf('|'))

  val letters = ('A' to 'Z').toList

  def directionToChar(direction: Direction): Char = direction match {
    case North | South => '|'
    case East | West => '-'
  }

  def getPathAt(x: Int, y: Int): Option[Char] = {
    if (x < 0 || x >= map.size || y < 0 || y >= map(x).size || map(x)(y) == ' ') {
      None
    }
    else {
      Some(map(x)(y))
    }
  }

  @tailrec
  def walkthrough(direction: Direction, x: Int, y: Int, count: Int, result: Seq[Char]): (Int, Seq[Char]) = {
    println(s"$x-$y")
    getPathAt(x, y) match {
      case None => (count , result)

      case Some(c) => c match {
        case '+' if direction == South && getPathAt(x, y - 1).exists(_ != ' ') =>
          walkthrough(West, x, y - 1, count + 1, result)
        case '+' if direction == South && getPathAt(x, y + 1).exists(_ != ' ') =>
          walkthrough(East, x, y + 1, count + 1, result)
        case '+' if direction == North && getPathAt(x, y - 1).exists(_ != ' ') =>
          walkthrough(West, x, y - 1, count + 1, result)
        case '+' if direction == North && getPathAt(x, y + 1).exists(_ != ' ') =>
          walkthrough(East, x, y + 1, count + 1, result)
        case '+' if direction == West && getPathAt(x - 1, y).exists(_ != ' ') =>
          walkthrough(North, x - 1, y, count + 1, result)
        case '+' if direction == West && getPathAt(x + 1, y).exists(_ != ' ') =>
          walkthrough(South, x + 1, y, count + 1, result)
        case '+' if direction == East && getPathAt(x - 1, y).exists(_ != ' ') =>
          walkthrough(North, x - 1, y, count + 1, result)
        case '+' if direction == East && getPathAt(x + 1, y).exists(_ != ' ') =>
          walkthrough(South, x + 1, y, count + 1, result)

        case c if direction == South =>
          walkthrough(direction, x + 1, y, count + 1, letters.find(_ == c).map(result :+ _).getOrElse(result))
        case c if direction == North =>
          walkthrough(direction, x - 1, y, count + 1, letters.find(_ == c).map(result :+ _).getOrElse(result))

        case c if direction == West =>
          walkthrough(direction, x, y - 1, count + 1, letters.find(_ == c).map(result :+ _).getOrElse(result))
        case c if direction == East =>
          walkthrough(direction, x, y + 1, count + 1, letters.find(_ == c).map(result :+ _).getOrElse(result))
      }

    }
  }

  sealed trait Direction {}

  case object North extends Direction

  case object South extends Direction

  case object East extends Direction

  case object West extends Direction

  val result = walkthrough(South, startingPoint._1, startingPoint._2, 0, Seq())

  println(result._2.mkString)
  println(result._1)

}
