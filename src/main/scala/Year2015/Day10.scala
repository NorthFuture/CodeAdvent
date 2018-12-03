
package Year2015

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {


  @tailrec
  def process(chars: Seq[Int], alreadySeen: Set[Seq[Int]], n: Int): Seq[Int] = {
    println(n)

    if (n == 0) {
      chars
    } else {

      if (alreadySeen.contains(chars)) {
        println("***")
        process(chars ++ chars, alreadySeen, n - 1)
      } else {

        val w = chars.par.foldLeft((chars.head, 0, List[Int]()))((s, x) => if (s._1 == x) s.copy(_2 = s._2 + 1) else (x, 1, s._3 :+ s._2 :+ s._1))

        alreadySeen + (chars)
        process(w._3 :+ w._2 :+ w._1, alreadySeen, n - 1)
      }
    }

  }

  process(Seq(1, 1, 1, 3, 1, 2, 2, 1, 1, 3), Set(), 50).foreach(print)

}