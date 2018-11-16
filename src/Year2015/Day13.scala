
package Year2015

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App {

  case class Fact(a: String, b: String, gain: Int)

  val regGain = """(\w*) would gain (\d*) happiness units by sitting next to (\w*).""".r
  val regLose = """(\w*) would lose (\d*) happiness units by sitting next to (\w*).""".r

  val facts = Source.fromFile("data/Year2015/day13.txt").getLines().map {
    case regGain(a, gain, b) => Fact(a, b, gain.toInt)
    case regLose(a, gain, b) => Fact(a, b, -gain.toInt)
  }.toList

  def getGain(a: String, b: String): Int = {
    if (a == "You" || b == "You") {
      0
    } else {
      facts.filter(x => (x.a == a && x.b == b) || (x.a == b && x.b == a)).map(_.gain).sum
    }
  }


  def arrangeTable(seated: Seq[String], toSeat: Seq[String]): Seq[Seq[String]] = {
    if (toSeat.isEmpty) {
      Seq(seated)
    } else {
      toSeat.flatMap(x => arrangeTable(seated :+ x, toSeat.filterNot(_ == x)))
    }
  }

  val persons = facts.map(_.a).distinct :+ "You"

  val setups = arrangeTable(Seq(persons.head), persons.tail).map(x =>
    x.foldLeft((x.head, Seq((x.head, x.last))))((s, x) => (x, s._2 :+ (s._1, x)))._2.map(x => getGain(x._1, x._2)).sum
  ).max

  println(setups)

}