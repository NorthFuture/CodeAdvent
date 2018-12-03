package Year2016

import scala.io.Source

object Day7_2 extends App {

  val regBracket ="""\[(.*?)\]""".r

  val input = Source.fromFile("data/Day7.txt").getLines().toList

  def isABA_3chars(chars: List[Char]): Boolean = {
    chars(0) != chars(1) && chars(0) == chars(2)
  }

  def getABA(chars: String): List[List[Char]] = {
    chars.toList.sliding(3).filter(isABA_3chars).toList
  }

  def isReversedABA(source: List[Char], dest: List[Char]): Boolean = {
    source(0) == dest(1) && dest(0) == source(1) && dest(2) == source(1)
  }

  val resultSSL = input.filter { x =>
    val bracketWordABAs = regBracket.findAllIn(x).toList.flatMap(getABA)

    val nonBracketWordABAs = regBracket.replaceAllIn(x, "#").split("#").toList.flatMap(getABA)

    nonBracketWordABAs.exists(z => bracketWordABAs.exists(x => isReversedABA(z, x)))
  }

  System.out.println(resultSSL.size)
}
