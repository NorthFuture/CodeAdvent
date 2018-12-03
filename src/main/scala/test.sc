import scala.annotation.tailrec
import scala.collection.mutable

val a = List(0, 1, 2, 3, 4, 5)


@tailrec
def combineSingle(pre: Seq[Int], post: Seq[Int], result: Seq[(Int, Seq[Int])]): Seq[(Int, Seq[Int])] = {

  post match {
    case Nil => result
    case h :: tail =>
      combineSingle(pre :+ h, tail, result :+ ((h, pre ++ tail)))
  }
}

def combine(list: Seq[Int], count: Int): Seq[Seq[Int]] = {

  if (count == 1) {
    list.sliding(1).toSeq
  } else {

    val o: Seq[(Int, Seq[Int])] = combineSingle(List(), list, Seq())

    o.flatMap(x => {
      combine(x._2, count - 1).map(y => x._1 :: Nil ++ y)
    })
  }

}

combine(a, 3)
