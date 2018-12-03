package Year2016

import Year2016.Day1.Direction

import scala.io.Source

object Day2 extends App {

  case class Position(current: Int) {

    def move(direction: Direction) = {
      val newCurrent = direction match {

        case Up => val x = this.current - 3
          if (x <= 0) {
            this.current
          } else {
            x
          }
        case Down =>
          val x = this.current + 3

          if (x > 9) {
            this.current
          } else {
            x
          }

        case Left =>
          val rowMin = (this.current - 1) / 3 * 3 + 1
          val x = this.current - 1

          if (x < rowMin) {
            rowMin
          } else {
            x
          }

        case Right =>
          val rowMax = (this.current - 1) / 3 * 3 + 3

          val x = this.current + 1
          if (x > rowMax) {
            rowMax
          } else {
            x
          }
      }

      System.out.print(newCurrent + " ")
      this.copy(current = newCurrent)
    }

  }

  val initialPosition = Position(5)
  val digits = Seq[Position]()

  val result = Source.fromFile("data/Day2Test.txt").getLines()
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

  System.out.println(result._2.map(_.current) mkString)

  case object Up extends Direction

  case object Right extends Direction

  case object Left extends Direction

  case object Down extends Direction

}
