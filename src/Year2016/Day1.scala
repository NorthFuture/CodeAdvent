package Year2016

import scala.io.Source

object Day1 extends App {

  case class Position(direction: Direction, x: Int, y: Int) {

    def move(steps: Int) = {
      val x = direction match {
        case North => this.copy(x = this.x - steps)
        case South => this.copy(x = this.x + steps)
        case West => this.copy(y = this.y - steps)
        case East => this.copy(y = this.y + steps)
      }

      System.out.println(x)
      x
    }

    def moveWithTracking(steps: Int) = {
      (1 to steps).map(move)

    }

    def turn(turnDirection: TurnDirection) = {

      val newDirection = (direction, turnDirection) match {
        case (North, Left) => West
        case (North, Right) => East

        case (East, Left) => North
        case (East, Right) => South

        case (South, Left) => East
        case (South, Right) => West

        case (West, Left) => South
        case (West, Right) => North
      }

      this.copy(direction = newDirection)
    }
  }

  val instructions = Source.fromFile("data/Day1.txt").getLines().flatMap(_.split(",")).map(_.trim).map { x =>
    Instruction(x.charAt(0) match {
      case 'R' => Right
      case 'L' => Left
    },
      x.substring(1).toInt
    )
  }.toList

  val initialPosition = Position(North, 0, 0)

  val finalPosition = instructions.foldLeft(initialPosition)((position: Position, instruction: Instruction) => {
    position.turn(instruction.turnDirection).move(instruction.steps)
  }
  )

  System.out.println(Math.abs(finalPosition.y) + Math.abs(finalPosition.x))

  val steps = instructions.foldLeft((initialPosition, Seq[Position]()))((state: (Position, Seq[Position]), instruction: Instruction) => {
    val x: Seq[Day1.Position] = state._1.turn(instruction.turnDirection).moveWithTracking(instruction.steps)

    (x.last, state._2 ++ x)
  }
  )._2

  val alreadyVisited = steps.foldLeft((Option.empty[Position], Set[Position]()))((state: (Option[Position], Set[Position]), position: Position) => {
    if (!state._1.isEmpty) {
      (state)
    } else if (state._1 == None && state._2.contains(position)) {
      (Some(position), Set[Position]())
    } else {
      (state._1, state._2.+(position))
    }
  })._1.get


  System.out.println(Math.abs(alreadyVisited.y) + Math.abs(alreadyVisited.x))

  trait TurnDirection

  case object Right extends TurnDirection

  case object Left extends TurnDirection

  case class Instruction(turnDirection: TurnDirection, steps: Int)

  trait Direction {
  }

  case object North extends Direction

  case object East extends Direction

  case object West extends Direction

  case object South extends Direction


}
