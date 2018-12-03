package Year2017

import scala.annotation.tailrec
import scala.io.Source

object Day22 extends App with Benchmark {

  import Day3.CenteredMatrix

  val input = Source.fromFile("data/Year2017/Day22.txt").getLines().toList
  val inputSize = input.size

  val matrix1 = new CenteredMatrix[Char](20000, '.')

  val matrix2 = new CenteredMatrix[Char](20000, '.')

  input.zipWithIndex.foreach(x => x._1.zipWithIndex.foreach(c => matrix1(inputSize / 2 - x._2, -inputSize / 2 + c._2) = c._1))
  input.zipWithIndex.foreach(x => x._1.zipWithIndex.foreach(c => matrix2(inputSize / 2 - x._2, -inputSize / 2 + c._2) = c._1))

  case class Point(x: Int, y: Int) {
    def move(direction: Direction): Point = {
      direction match {
        case North => this.copy(x = this.x + 1)
        case East => this.copy(y = this.y + 1)
        case South => this.copy(x = this.x - 1)
        case West => this.copy(y = this.y - 1)
      }
    }
  }

  object Point {
    implicit def pointToTuple(point: Point): (Int, Int) = (point.x, point.y)
  }

  sealed trait Direction {

    def turnRight(): Direction = this match {
      case North => East
      case East => South
      case South => West
      case West => North
    }

    def turnLeft(): Direction = this match {
      case North => West
      case West => South
      case South => East
      case East => North
    }

  }

  case object North extends Direction

  case object South extends Direction

  case object East extends Direction

  case object West extends Direction


  @tailrec
  def process(matrix: CenteredMatrix[Char], pos: Point, direction: Direction, count: Int, countInfection: Int): Int = {
    if (count == 0) {
      countInfection
    } else {
      val newDirection = matrix(pos) match {
        case '#' => direction.turnRight()
        case '.' => direction.turnLeft()
      }

      val newCountInfection = matrix(pos) match {
        case '#' => matrix(pos.x, pos.y) = '.'; countInfection
        case '.' => matrix(pos.x, pos.y) = '#'; countInfection + 1
      }

      val newPos = pos.move(newDirection)

      process(matrix, newPos, newDirection, count - 1, newCountInfection)
    }
  }


  @tailrec
  def process2(matrix: CenteredMatrix[Char], pos: Point, direction: Direction, count: Int, countInfection: Int): Int = {
    if (count == 0) {
      countInfection
    } else {
      val newDirection = matrix(pos) match {
        case '#' => direction.turnRight()
        case '.' => direction.turnLeft()
        case 'W' => direction
        case 'F' => direction.turnLeft().turnLeft()
      }

      val newCountInfection = matrix(pos) match {
        case '#' => matrix(pos.x, pos.y) = 'F'; countInfection
        case '.' => matrix(pos.x, pos.y) = 'W'; countInfection
        case 'F' => matrix(pos.x, pos.y) = '.'; countInfection
        case 'W' => matrix(pos.x, pos.y) = '#'; countInfection + 1

      }

      val newPos = pos.move(newDirection)

      process2(matrix, newPos, newDirection, count - 1, newCountInfection)
    }
  }

  val result = process(matrix1, Point(0, 0), North, 10000, 0)

  println(result)

  val result2 = process2(matrix2, Point(0, 0), North, 10000000, 0)

  println(result2)


}
