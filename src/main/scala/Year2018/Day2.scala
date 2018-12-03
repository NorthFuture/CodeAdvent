package Year2018

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day2 extends App {

  def getInput() = Source.fromFile("data/Year2018/Day2.txt").getLines().toList

  def scan(s: String): (Boolean, Boolean) = {
    val g = s.groupBy(x => x)

    (g.exists(_._2.length == 2), g.exists(_._2.length == 3))
  }

  val input = getInput()

  val r1 = input.map(scan).map(x => (if (x._1) 1 else 0, if (x._2) 1 else 0))
    .foldLeft((0, 0))((s, x) => (s._1 + x._1, s._2 + x._2))

  println(r1._1 * r1._2)

  def solveO_NQuadratic_MLongest() = {
    def compare(s1: String, s2: String): Option[String] = {
      val g = s1.zip(s2).foldLeft(("", "", ""))((s, x) => if (x._1 == x._2) s.copy(_1 = s._1 + x._1) else s.copy(_2 = s._2 + x._1, _3 = s._3 + x._2))

      if (g._2.length == 1 && g._3.length == 1) {
        Some(g._1)
      } else {
        None
      }
    }

    input.flatMap(x => input.map(y => (x, y))).flatMap(x => compare(x._1, x._2))
  }

  def distance(s1: String, s2: String): Int = s1.zip(s2).foldLeft(0)((s, x) => s + Math.abs(x._2 - x._1))

  def solveO() = {

    val len=input.head.length


    val d = mutable.Map[(String, String), Int]()

    @tailrec
    def calcD(current: String, input: List[String]): Unit = {
      input match {
        case h :: tail =>
          d += (((current, h), distance(current, h)))

          calcD(h, tail)
        case Nil =>
      }
    }

    calcD(input.head, input.tail)

    d
  }

  solveO().foreach(println)


}

