
package Year2015

import scala.io.Source

object Day2 {

  def input = Source.fromFile("data/Year2015/day2.txt").getLines()

  val regExp = """(\d*?)x(\d*?)x(\d*?)""".r

  def Step1() {

    val r = input.map { x =>
      x match {
        case regExp(wS, lS, hS) => {
          val w = wS.toInt
          val l = lS.toInt
          val h = hS.toInt

          val wl = w * l
          val wh = w * h
          val lh = l * h

          val min = wl.min(wh).min(lh)

          wl * 2 + wh * 2 + lh * 2 + min
        }
      }
    }

    println(r.sum)
  }

  def Step2() {

    val r = input.map { x =>

      x match {
        case regExp(wS, lS, hS) => {
          val w = wS.toInt
          val l = lS.toInt
          val h = hS.toInt

          val wl = 2 * w + 2 * l
          val wh = 2 * w + 2 * h
          val lh = 2 * l + 2 * h

          val extra = w * l * h

          wl.min(wh).min(lh) + extra
        }
      }
    }

    println(r.sum)
  }

  def main(args: Array[String]): Unit = {
    Step1()
    Step2();
  }
}