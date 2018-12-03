package Year2016

import scala.annotation.tailrec
import scala.io.Source

object Day9 extends App {

  val reg ="""([A-Z]*?)\((\d+?)x(\d+?)\)(.*)""".r

  val reg2 ="""\((\d+?)x(\d+?)\)(.*)""".r

  val input = Source.fromFile("data/Day9.txt").getLines().toList

  def processString(x: String): String = {

    @tailrec
    def internalProcessString(x: String, result: String): String = {

      if (x.isEmpty) {
        result
      } else {

        x match {
          case reg(char_count, repeat_count, tail) =>
            val chunks = tail.splitAt(char_count.toInt)

            internalProcessString(chunks._2, result + chunks._1 * repeat_count.toInt)
          case _ => internalProcessString(x.tail, result + x.head)
        }

      }

    }

    internalProcessString(x, "")
  }

  def processString3(x: String): Long = {

    def getPartialChunks(x: String): (String, Int, String, String) = {

      x match {
        case reg(head, char_count, repeat_count, tail) =>

          val tmp_tail = tail.splitAt(char_count.toInt)

          val to_repeat = tmp_tail._1

          val real_tail = tmp_tail._2
          (head, repeat_count.toInt, to_repeat, real_tail)

        case l: String => (l, 0, "", "")
      }

    }

    //@tailrec
    def internalProcess(x: String): Long = {

      if (x.isEmpty) {
        0
      } else {
        val r = getPartialChunks(x)

        val repeatLength = internalProcess(r._3) * r._2

        (r._1.length + repeatLength + internalProcess(r._4))
      }

    }

    internalProcess(x)
  }


  input.map(processString3).foreach(x => println(x == 11107527530L))

}
