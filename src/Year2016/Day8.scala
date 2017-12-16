package Year2016

import scala.io.Source

object Day8 extends App {

  val rectReg ="""rect (\d+)x(\d+)""".r

  val rotateColReg ="""rotate column x=(\d+) by (\d+)""".r

  val rotateRowReg ="""rotate row y=(\d+) by (\d+)""".r

  val SCREEN_WIDTH = 50
  val SCREEN_HEIGHT = 6

  val intialScreen = Screen()

  val finalScreen = Source.fromFile("data/Day8.txt").getLines().map { x =>
    x match {
      case rectReg(cols, rows) => Rect(cols.toInt, rows.toInt)
      case rotateColReg(c, by) => RotateCol(c.toInt, by.toInt)
      case rotateRowReg(r, by) => RotateRow(r.toInt, by.toInt)
      case _ => System.out.println("Malformed " + x); Nop
    }
  }
    .foldLeft(intialScreen)((s, i) => s.processInstruction(i))


  finalScreen.display()

  val result = finalScreen.data.flatMap(x => x).filter(x => x).length

  System.out.println(result)

  case class Screen(data: Array[Array[Boolean]] = Array.fill(SCREEN_HEIGHT, SCREEN_WIDTH)(false)) {


    def display() = {
      data.foreach(r => {
        r.foreach(c => print(if (c) "#" else " "))
        println("")
      })
      println("-----------------------------")
      this
    }

    def processInstruction(i: Instruction): Screen = {


      i match {
        case rect: Rect =>
          val newData = data.clone()

          for {
            r <- 0 to rect.rows - 1
            c <- 0 to rect.cols - 1
          } newData(r)(c) = true

          Screen(newData)

        case rotateCol: RotateCol =>

          val newData = data.transpose

          val chunk1 = newData(rotateCol.c).slice(0, SCREEN_HEIGHT - rotateCol.by)
          val chunk2 = newData(rotateCol.c).slice(SCREEN_HEIGHT - rotateCol.by, SCREEN_HEIGHT)

          (chunk2 ++ chunk1).copyToArray(newData(rotateCol.c))

          Screen(newData.transpose)

        case rotateRow: RotateRow =>
          val newData = data

          val chunk1 = newData(rotateRow.r).slice(0, SCREEN_WIDTH - rotateRow.by)
          val chunk2 = newData(rotateRow.r).slice(SCREEN_WIDTH - rotateRow.by, SCREEN_WIDTH)

          (chunk2 ++ chunk1).copyToArray(newData(rotateRow.r))

          Screen(newData)

          this
        case _ => this
      }
    }
  }

  sealed trait Instruction

  case object Nop extends Instruction

  case class Rect(cols: Int, rows: Int) extends Instruction

  case class RotateCol(c: Int, by: Int) extends Instruction

  case class RotateRow(r: Int, by: Int) extends Instruction

}
