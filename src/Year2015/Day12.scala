
package Year2015

import scala.io.Source

object Day12 extends App {

  val reg = """(\-?\d*)""".r

  val regClean = """\{.*?:"red".*?\}"""

  val data = Source.fromFile("data/Year2015/day12.txt").getLines().toList.head


  println(reg.findAllIn(data).filterNot(_.isEmpty).map(_.toInt).sum)

  val result=data.replaceAll(regClean,"")

  println(reg.findAllIn(result).filterNot(_.isEmpty).map(_.toInt).sum)

}