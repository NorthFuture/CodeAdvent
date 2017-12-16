package Year2016

import scala.io.Source

object Day7 extends App {

  val regBracket ="""\[(.*?)\]""".r

  val input = Source.fromFile("data/Day7.txt").getLines().toList

  def isABBA_4chars(chars: List[Char]): Boolean = {
    chars(0) != chars(1) && chars(0) == chars(3) && chars(1) == chars(2)
  }

  def isABBA(chars: String): Boolean = {
    chars.toList.sliding(4).exists(isABBA_4chars)
  }

  val resultTLS = input.filter { x =>
    val bracketWords = regBracket.findAllIn(x).toList

    val nonBracketWords = regBracket.replaceAllIn(x, "#").split("#").toList

    !bracketWords.exists(isABBA) && nonBracketWords.exists(isABBA)
  }

  System.out.println(resultTLS.size)

}
