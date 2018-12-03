package Year2017

import scala.io.Source

object Day9 extends App with Benchmark {

  // 7616-3838
  case class State(currentScoreLevel: Int, totaleScore: Int, inGarbage: Boolean, garbaceCount: Int, charDelete: Boolean)

  val inputs = Source.fromFile("data/Year2017/Day9.txt").getLines()
    .map(x => (x, x.foldLeft(State(0, 0, false, 0, false))((s, c) =>
      c match {
        case '!' if !s.charDelete =>
          s.copy(charDelete = true)
        case _ if s.charDelete =>
          s.copy(charDelete = false)
        case '}' if !s.inGarbage =>
          s.copy(currentScoreLevel = s.currentScoreLevel - 1, totaleScore = s.totaleScore + s.currentScoreLevel)
        case '{' if !s.inGarbage =>
          s.copy(currentScoreLevel = s.currentScoreLevel + 1)
        case _ if s.inGarbage && c != '>' =>
          s.copy(garbaceCount = s.garbaceCount + 1)
        case '<' =>
          s.copy(inGarbage = true)
        case '>' =>
          s.copy(inGarbage = false)
        case _ if !s.inGarbage => s
      }
    )
    ))
    .map(x => x._1 + "->" + x._2.totaleScore + "-" + x._2.garbaceCount)
    .foreach(println)


  def calculateTotalScore(block: Block): Int = {
    block.innerBlocks.filter(!_.isGarbage).foldLeft(block.score)((s, i) =>
      s + calculateTotalScore(i)
    )
  }

  case class Block(score: Int, isGarbage: Boolean, content: String, innerBlocks: Seq[Block])

}

