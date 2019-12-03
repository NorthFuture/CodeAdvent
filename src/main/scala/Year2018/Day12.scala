package Year2018

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day12 extends App {

  val reg ="""(.*) => (.)""".r

  // val input = "#..#.#..##......###...###".map(x => x == '#')
  val input = "##.####..####...#.####..##.#..##..#####.##.#..#...#.###.###....####.###...##..#...##.#.#...##.##..".map(x => x == '#')

  val patterns = Source.fromFile("data/Year2018/Day12.txt").getLines().map(x => x.trim match {
    case reg(a, b) => a.map(_ == '#').toList -> (b == "#")
  }).toMap

  val pots = input.zipWithIndex.foldLeft(mutable.Set[Int]())((s, x) => if (x._1) s + (x._2) else s)

  def tick(pots: mutable.Set[Int]): mutable.Set[Int] = {

    val min = pots.min
    val max = pots.max

    val newPots = mutable.Set[Int]()

    for (i <- min - 2 to max + 2) {
      val s = List(pots(i - 2), pots(i - 1), pots(i), pots(i + 1), pots(i + 2))

      if (patterns.getOrElse(s, false)) {
        newPots.update(i, true)
      }
    }
    newPots
  }

  def printPots(min: Int, pots: mutable.Set[Int]): String = {
    (min to pots.max).map(x => if (pots(x)) "#" else ".").mkString("")
  }

  def stream(i: Long = 1): Stream[Long] = i #:: stream(i + 1)

  @tailrec
  def generations(count: Long, pots: mutable.Set[Int], history: Seq[Int]): mutable.Set[Int] = {
    if (count == 0) {
      pots
    } else {
      println(pots.sum)
      generations(count - 1, tick(pots), history :+ pots.sum)
    }
  }

  val lastGeneration = generations(50000000000L, pots, Seq())
  val result = lastGeneration.sum

  //println(generations.map(printPots(-5,_)).mkString("\n"))

  println(result)
}

