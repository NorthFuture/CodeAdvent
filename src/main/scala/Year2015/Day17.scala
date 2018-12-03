
package Year2015

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day17 extends App {

  case class Container(id: Int, capacity: Int) {
  }

  val containers = Source.fromFile("data/Year2015/tday17.txt").getLines().zipWithIndex.map(x => Container(x._2, x._1.toInt)).toList

  val total = 150


  @tailrec
  def combineSingle(pre: Seq[Container], post: Seq[Container], result: Seq[(Container, Seq[Container])]): Seq[(Container, Seq[Container])] = {

    post match {
      case Nil => result
      case h :: tail =>
        combineSingle(pre :+ h, tail, result :+ ((h, pre ++ tail)))
    }
  }

  def combine(list: Seq[Container], count: Int): Seq[Seq[Container]] = {

    if (count == 1) {
      list.sliding(1).toSeq
    } else {

      val o = combineSingle(List(), list, Seq())

      o.flatMap(x => {
        combine(x._2, count - 1).map(y => x._1 :: Nil ++ y)
      })
    }

  }

  @tailrec
  def solve(state: Seq[(Set[Container], Set[Container])], res: mutable.ListBuffer[Set[Container]]): Seq[Set[Container]] = {

    val r = state.flatMap {
      s =>
        s._1.map {
          c =>
            val newCollection = s._2 + c

            newCollection.map(_.capacity).sum match {
              case `total` =>

                res.+=(newCollection)

                None
              case x if x < total =>
                Some(s._1.filterNot(_ == c), newCollection)
              case _ =>
                None
            }
        }
    }


    if (!r.isEmpty) {
      solve(r.flatten, res)
    } else {
      res
    }

  }

  val result = combine(containers, containers.size).distinct

  println(result.size)
}