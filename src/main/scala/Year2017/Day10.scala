package Year2017

import scala.annotation.tailrec
import scala.collection.Seq

object Day10 extends App with Benchmark {

  val _input = (4, Seq(3, 4, 1, 5))

  val input = (255, Seq(34, 88, 2, 222, 254, 93, 150, 0, 199, 255, 39, 32, 137, 136, 1, 167))
  val input2 = (255, "34, 88, 2, 222, 254, 93, 150, 0, 199, 255, 39, 32, 137, 136, 1, 167")

  case class State(list: List[Int], currentPosition: Int, skip: Int)

  @tailrec
  def takeCircularList(list: List[Int], totalTakeCount: Int, takeCount: Int, currentPosition: Int, part1: List[Int], part2: List[Int]): (List[Int], List[Int]) = {

    takeCount match {
      case x if x > 0 => takeCircularList(list, totalTakeCount - 1, takeCount - 1, currentPosition + 1, part1 :+ list(currentPosition % list.size), part2)
      case _ if totalTakeCount > 0 => takeCircularList(list, totalTakeCount - 1, takeCount - 1, currentPosition + 1, part1, part2 :+ list(currentPosition % list.size))
      case _ => (part1, part2)
    }
  }

  def shiftList(list: List[Int], count: Int): List[Int] = {

    @tailrec
    def internal(list: List[Int], totalLength: Int, currentPosition: Int, tailResult: List[Int], headResult: List[Int]): List[Int] = {

      list match {
        case Nil =>
          headResult ::: tailResult
        case h :: tail if currentPosition >= totalLength =>
          internal(tail, totalLength, currentPosition + 1, tailResult, headResult :+ h)
        case h :: tail =>
          internal(tail, totalLength, currentPosition + 1, tailResult :+ h, headResult)
      }
    }

    internal(list, list.length, count, List(), List())
  }

  def process(s: State, e: Int): State = {
    val (part1, part2) = takeCircularList(s.list, s.list.length, e, s.currentPosition, List(), List())

    val r = shiftList(part1.reverse ::: part2, s.currentPosition)

    State(r, (s.currentPosition + s.skip + e) % s.list.length, s.skip + 1)
  }

  val steps1 = input._2.scanLeft(State(0 to input._1 toList, 0, 0))(process)

  steps1.map(s => s.list.zipWithIndex.map(x => if (x._2 == s.currentPosition) "(" + x._1 + ")" else " " + x._1.toString + " ").mkString(" "))
    .foreach(println)

  val resultList = steps1.last.list.take(2)

  println(resultList(0) * resultList(1))

  val steps2 = input2._2.filter(_ != ' ').map(_.toInt) ++: Seq(17, 31, 73, 47, 23)

  val r = (0 to 63).flatMap(x => steps2)
    .foldLeft(State(0 to input._1 toList, 0, 0))(process)
    .list.grouped(16).map(x => x.tail.foldLeft(x.head)((s, n) => s ^ n))
    .map(_.formatted("%02x")).mkString("")

  println(r)
}

