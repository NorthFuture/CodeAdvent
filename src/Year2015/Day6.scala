
package Year2015

import scala.io.Source

object Day6 {

  def input = Source.fromFile("day6.txt").getLines()

  val reg = """(turn off|turn on|toggle) (\d*),(\d*) through (\d*),(\d*)""".r

  def Step1() {

    val lights = Array.ofDim[Boolean](1000, 1000)

    val r = input.foreach { x =>

      x match {
        case reg(action, startxS, startyS, endxS, endyS) =>

          val startx = startxS.toInt
          val starty = startyS.toInt

          val endx = endxS.toInt
          val endy = endyS.toInt

          val aa = for {
            i <- startx to endx
            j <- starty to endy
          } yield (i, j)

          action match {
            case "toggle" => aa.foreach { x => lights(x._1).update(x._2, !lights(x._1)(x._2)) }
            case "turn off" => aa.foreach { x => lights(x._1).update(x._2, false) }
            case "turn on" => aa.foreach { x => lights(x._1).update(x._2, true) }
          }
      }
    }

    val t = for {
      i <- 0 to 999
      j <- 0 to 999
    } yield (i, j)

    val res = t.count { x => lights(x._1)(x._2) }

    println(s"$res")
  }

  def Step2() {

    val lights = Array.ofDim[Int](1000, 1000)

    val r = input.foreach { x =>

      x match {
        case reg(action, startxS, startyS, endxS, endyS) =>

          val startx = startxS.toInt
          val starty = startyS.toInt

          val endx = endxS.toInt
          val endy = endyS.toInt

          val aa = for {
            i <- startx to endx
            j <- starty to endy
          } yield (i, j)

          aa.foreach { x =>
            val newBright = action match {
              case "toggle" => lights(x._1)(x._2) + 2
              case "turn on" => lights(x._1)(x._2) + 1
              case "turn off" => Seq(0, lights(x._1)(x._2) -1).max
            }

            lights(x._1).update(x._2, newBright)
          }
     
      }
    }

    val t = for {
      i <- 0 to 999
      j <- 0 to 999
    } yield lights(i)(j)

    val res = t.sum

    println(s"$res")
  }

  def main(args: Array[String]): Unit = {
    Step2()
  }
}