package Year2016

import Year2016.Day1.Direction

import scala.io.Source

object Day2_2 extends App {

  val allowedMatrix = Array(
    Array(-1, -1, 1, -1, -1),
    Array(-1, 2, 3, 4, -1),
    Array(5, 6, 7, 8, 9),
    Array(-1, 10, 11, 12, -1),
    Array(-1, -1, 13, -1, -1)
  )

  val indexes = Map(
    1 -> (0, 2),
    2 -> (1, 1),
    3 -> (1, 2),
    4 -> (1, 3),
    5 -> (2, 0),
    6 -> (2, 1),
    7 -> (2, 2),
    8 -> (2, 3),
    9 -> (2, 4),
    10 -> (3, 1),
    11 -> (3, 2),
    12 -> (3, 3),
    13 -> (4, 2)
  )

  // A,B,C,D are 10,11,12,13
  case class Position(current: Int) {

    def move(direction: Direction) = {

      val currentMatrixPosition = indexes(current)

      val newMatrixPosition = direction match {
        case Up => (currentMatrixPosition._1 - 1, currentMatrixPosition._2)
        case Down => (currentMatrixPosition._1 + 1, currentMatrixPosition._2)
        case Left => (currentMatrixPosition._1, currentMatrixPosition._2 - 1)
        case Right => (currentMatrixPosition._1, currentMatrixPosition._2 + 1)
      }

      val newMatrixPositionAllowed = if (newMatrixPosition._1 < 0 || newMatrixPosition._1 > 4 || newMatrixPosition._2 < 0 || newMatrixPosition._2 > 4) {
        currentMatrixPosition
      } else {
        newMatrixPosition
      }

      val newPosition = allowedMatrix(newMatrixPositionAllowed._1)(newMatrixPositionAllowed._2)

      val result = if (newPosition == -1) this else Position(newPosition)

      System.out.print(result.current.toLong.toHexString + " ")

      result
    }

  }

  val initialPosition = Position(5)
  val digits = Seq[Position]()

  val result = Source.fromFile("data/Day2.txt").getLines()
    .map {
      _.map {
        _ match {
          case 'U' => Up
          case 'R' => Right
          case 'L' => Left
          case 'D' => Down
        }
      }
    }
    .foldLeft((initialPosition, digits)) { (state, instruction) =>
      val oneSetFinalPosition: Position = instruction.foldLeft(state._1) { (position, instruction) => position.move(instruction) }

      System.out.println("** " + oneSetFinalPosition)
      state.copy(_1 = oneSetFinalPosition, _2 = state._2 :+ oneSetFinalPosition)
    }

  System.out.println(result._2.map(_.current.toLong.toHexString) mkString)

  case object Up extends Direction

  case object Right extends Direction

  case object Left extends Direction

  case object Down extends Direction

}
