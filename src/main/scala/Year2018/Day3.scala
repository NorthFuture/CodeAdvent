package Year2018

import scala.collection.mutable
import scala.io.Source

object Day3 extends App {

  case class Claim(id: Int, left: Int, top: Int, width: Int, height: Int) {
    def filled(x: Int, y: Int): Boolean = x >= left && x <= left + width && y >= top && y <= top + height
  }

  val reg ="""#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  val input = Source.fromFile("data/Year2018/Day3.txt").getLines().map { x =>
    x match {
      case reg(id, left, top, width, height) => Claim(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
    }
  }.toList

  val result = input.flatMap(c => for {
    x <- c.left to c.left + c.width - 1
    y <- c.top to c.top + c.height - 1
  } yield ((x, y), c.id))
    .foldLeft(Map[(Int, Int), Int]())((s, x) => s.get(x._1) match {
      case None => s + ((x._1, x._2))
      case Some(_) => s + ((x._1, -1))
    }).count(x => x._2 == -1)

  val result2 = input.flatMap(c => for {
    x <- c.left to c.left + c.width - 1
    y <- c.top to c.top + c.height - 1
  } yield ((x, y), c.id))
    .foldLeft((input.map(_.id).toSet, Map[(Int, Int), Int]()))((s, x) => s._2.get(x._1) match {
      case None => (s._1, s._2 + ((x._1, x._2)))
      case Some(e) => (s._1 - (e, x._2), s._2 + ((x._1, -1)))
    })._1

  println(result)
  println(result2)
}

