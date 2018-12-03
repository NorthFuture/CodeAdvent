
package Year2015

import scala.io.Source

object Day8 extends App {

  def input = Source.fromFile("data/Year2015/day8.txt").getLines()

  def processString(s: String): (Int, Int) = {

    case class State(total: Int, chars: Int, inEscape: Boolean, escapeCount: Option[Int])


    val result = s.foldLeft(State(0, 0, false, None))((s, c) => {


      c match {
        case '"' | '\\' if s.inEscape =>
          s.copy(total = s.total + 1, chars = s.chars + 1, inEscape = false)

        case '"' if !s.inEscape =>
          print("*"); s.copy(total = s.total + 1, chars = s.chars, inEscape = false)

        case '\\' if !s.inEscape =>
          s.copy(total = s.total + 1, chars = s.chars, inEscape = true)

        case 'x' if s.inEscape =>
          s.copy(total = s.total + 1, chars = s.chars, inEscape = true, escapeCount = Some(2))

        case _ if s.inEscape && s.escapeCount.exists(_ > 1) =>
          s.copy(total = s.total + 1, chars = s.chars, inEscape = true, escapeCount = s.escapeCount.map(_ - 1))

        case _ if s.inEscape && s.escapeCount.exists(_ == 1) =>
          s.copy(total = s.total + 1, chars = s.chars + 1, inEscape = false, escapeCount = None)

        case _ =>
          s.copy(total = s.total + 1, chars = s.chars + 1, false, escapeCount = None)
      }
    }
    )
    (result.total, result.chars)

  }

  def processString2(s: String): String = {
    "\"" + s.flatMap(c => c match {
      case '"' => "\\\""
      case '\\' => "\\\\"
      case x: Char => x.toString
    }) + "\""

  }

  val result = input.map(processString)
    .foldLeft((0, 0))((a, e) => (a._1 + e._1, a._2 + e._2))

  println(result)
  println(result._1 - result._2)


  val l1 = input.map(processString2).map(_.length).sum
  val l2 = input.map(_.length).sum

   println(l1 - l2)
}