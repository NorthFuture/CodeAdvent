package Year2017

import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {

  def process(s: String): Int = {

    @tailrec
    def groupMatchingChars(currentGroup: Seq[Char], string: String, groups: Stream[Seq[Char]]): Stream[Seq[Char]] = {
      if (string.isEmpty) {
        groups match {
          case h #:: t if h.head == currentGroup.head => (groups.head ++ currentGroup) #:: groups.tail
          case _ => groups #::: currentGroup #:: Stream.empty[Seq[Char]]
        }
      } else {
        if (currentGroup.head == string.head) {
          groupMatchingChars(currentGroup :+ string.head, string.tail, groups)
        } else {
          groupMatchingChars(Seq(string.head), string.tail, groups :+ currentGroup)
        }
      }
    }

    groupMatchingChars(Seq(s.head), s.tail, Stream.empty).filterNot(_.tail.isEmpty).map(_.head - '0').sum
  }

  Source.fromFile("data/Year2017/Day1.txt").getLines().map(s => (s + s.head).sliding(2).filter(x => x.head == x.last).map(_.head - '0').sum)
    .foreach(println)
}
