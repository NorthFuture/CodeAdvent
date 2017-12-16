
package codeadvent

import scala.io.Source
import java.util.regex.Matcher
import java.util.regex.Pattern

object Day5 {

  def input = Source.fromFile("day5.txt").getLines()

  def Step1() {

    val r = input.filter { x =>

      val badwords = List("ab", "cd", "pq", "xy")
      val vovels = List('a', 'e', 'i', 'o', 'u')

      val rule1 = badwords.filter { b => x.indexOf(b) != -1 }.isEmpty

      val rule2 = x.filter { x => vovels.indexOf(x) != -1 }

      val rule3 = (0 to 26).map { w =>
        val c = (w + 'a'.toInt).toChar
        val s = c.toString() + c.toString()
        x.indexOf(s) != -1
      }.filter { x => x }

      val r = (rule1 && rule2.size > 2 && rule3.size > 0)

      if (r) {
        println(s"good $x")
      } else {
        println(s"bad $x")
      }

      r
    }.size

    println(s"$r")

  }

  def Step2() {

    val r = input.filter { x =>

      val r1 = (0 to (x.length() - 2))
        .filter { i =>
          val pair = x.substring(i, i + 2)

          val left = x.substring(0, Seq(i, 0).max)

          val right = x.substring(Seq(i + 3,x.length()).min, x.length())

          left.indexOf(pair) != -1 || right.indexOf(pair) != -1
        }

      val r2 = (0 to (x.length() - 3)).map { i =>
        x.substring(i, i + 3)
      }.filter { x => x.charAt(0) == x.charAt(2) }

      val r = r1.size > 0 && r2.size > 0

      if (r) {
        println(s"good $x")
      } else {
        println(s"bad $x")
      }

      r
    }.size

    println(s"$r")

  }

  def main(args: Array[String]): Unit = {
    Step2()
  }
}