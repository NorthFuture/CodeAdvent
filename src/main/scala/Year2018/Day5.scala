package Year2018

import scala.annotation.tailrec
import scala.io.Source

object Day5 extends App {

  def process(s: String): String = {

    def isMatching(left: Char, right: Char): Boolean = {
      (left.isUpper && right.isLower && left == right.toUpper) ||
        (left.isLower && right.isUpper && left == right.toLower)
    }

    @tailrec
    def internal(left: String, right: String): String = {
      // left is reversed

      if (right.isEmpty) {
        left.reverse
      } else if (left.isEmpty) {
        internal(right.head.toString, right.tail)
      } else {
        if (isMatching(left.head, right.head)) {
          internal(left.tail, right.tail)
        } else {
          internal(right.head + left, right.tail)
        }
      }
    }

    internal("", s)
  }

  val input = Source.fromFile("data/Year2018/Day5.txt").getLines().toList

  input.map(process).map(_.length).foreach(println)

  input.map { i =>
    val replaced = 'a' to 'z' map { c => process(i.filterNot(y => y == c || y.toLower == c)) }

    replaced.minBy(_.length)
  }.map(_.length).foreach(println)
}
