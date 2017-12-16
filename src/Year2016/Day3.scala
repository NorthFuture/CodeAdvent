package Year2016

import scala.io.Source

object Day3 extends App {

  val reg ="""\s*(\d+)\s*(\d+)\s*(\d+)\s*""".r

  def isValid(x: Seq[Int]): Boolean = x(0) + x(1) > x(2) && x(0) + x(2) > x(1) && x(1) + x(2) > x(0)

  val lines = Source.fromFile("data/Day3.txt").getLines().map { x =>
    x match {
      case reg(n1, n2, n3) => Seq(n1.toInt, n2.toInt, n3.toInt)
      case _ => System.out.println(s"Malformed ${x}"); Seq()
    }

  }.toList

  val count1 = lines.filter(isValid).length

  val count2 = lines.sliding(3, 3).toList.map(x => x.transpose).flatMap(x => x).filter(isValid).length
  System.out.println(count1)
  System.out.println(count2)

}
