
package Year2015

import scala.io.Source

object Day16 extends App {

  val reg = """Sue (\d*): (\w*): (\d*), (\w*): (\d*), (\w*): (\d*)""".r

  case class Aunt(name: String, compunds: Map[String, Int]) {
  }

  val inputs = Source.fromFile("data/Year2015/day16.txt").getLines().map {
    case reg(name, a, b, c, d, e, f) => Aunt(name, Map(
      a -> b.toInt,
      c -> d.toInt,
      e -> f.toInt
    ))
  }.toList

  def matchCompound(v: Option[Int], target: Int) = v match {
    case None => true
    case Some(x) => x == target
  }

  def matchCompoundl(v: Option[Int], target: Int) = v match {
    case None => true
    case Some(x) => x < target
  }

  def matchCompoundg(v: Option[Int], target: Int) = v match {
    case None => true
    case Some(x) => x > target
  }

  val r = inputs.filter(x =>
    matchCompound(x.compunds.get("children"), 3) &&

      matchCompoundg(x.compunds.get("cats"), 7) &&
      matchCompoundg(x.compunds.get("trees"), 3) &&

      matchCompound(x.compunds.get("samoyeds"), 2) &&

      matchCompoundl(x.compunds.get("pomeranians"), 3) &&
      matchCompoundl(x.compunds.get("goldfish"), 5) &&


      matchCompound(x.compunds.get("akitas"), 0) &&
      matchCompound(x.compunds.get("vizslas"), 0) &&
      matchCompound(x.compunds.get("cars"), 2) &&
      matchCompound(x.compunds.get("perfumes"), 1)
  )

  r.foreach(println)

}