package Year2017

import scala.annotation.tailrec
import scala.io.Source

object Day6 extends App with Benchmark {

  val initialBanksTest = Map(0 -> 0, 1 -> 2, 2 -> 7, 3 -> 0)

  val initialBanks = Map(0 -> 0, 1 -> 5, 2 -> 10, 3 -> 0, 4 -> 11, 5 -> 14, 6 -> 13, 7 -> 4, 8 -> 11, 9 -> 8, 10 -> 8, 11 -> 7, 12 -> 1, 13 -> 4, 14 -> 12, 15 -> 11)

  @tailrec
  def recursiveDistributor(banks: Map[Int, Int], currentBlock: Int, blocksCount: Int): Map[Int, Int] = {

    if (blocksCount == 0) {
      banks
    } else {
      val normCurrentBlock = currentBlock % banks.size
      val newBanks = banks + (normCurrentBlock -> (banks(normCurrentBlock) + 1))
      recursiveDistributor(newBanks, currentBlock + 1, blocksCount - 1)
    }
  }

  def optimizedDistributor(banks: Map[Int, Int], currentBlock: Int, blocksCount: Int): Map[Int, Int] = {
    val banksSize = banks.size

    val normCurrentBlock = currentBlock % banksSize

    val blocksCountModulo = blocksCount % banksSize

    val entireBanksIncrement = blocksCount / banksSize

    val availableTailLength = banksSize - normCurrentBlock

    val tailStartIndex = normCurrentBlock
    val tailEndIndex = math.min(banksSize, tailStartIndex + blocksCountModulo)

    val headEndIndex = blocksCountModulo - (tailEndIndex - tailStartIndex)

    val result = banks.map { case (key, value) => (key,
      if (((key >= tailStartIndex && key < tailEndIndex) || key < headEndIndex)) {
        value + entireBanksIncrement + 1
      } else {
        value + entireBanksIncrement
      })
    }

    result
  }

  @tailrec
  def balanceBanks(banks: Map[Int, Int], alreadySeen: Seq[Map[Int, Int]], distributor: (Map[Int, Int], Int, Int) => Map[Int, Int]): Seq[Map[Int, Int]] = {

    //implicit val ordering: Ordering[(Int, Int)] = Ordering.by(y => y._2 * 100 - y._1)

    val maxBank = banks.maxBy(x => (x._2, -x._1))

    val newBanks = distributor(banks + (maxBank._1 -> 0), maxBank._1 + 1, maxBank._2)

    alreadySeen.find(_ == newBanks) match {
      case Some(x) => alreadySeen :+ newBanks
      case _ => balanceBanks(newBanks, alreadySeen :+ newBanks, distributor)
    }

  }

  def print(x: Map[Int, Int]) = {
    println(x.toList.sortBy(_._1).map(_._2.formatted("%4d")).mkString(""))
  }

  benchmark {
    val result = balanceBanks(initialBanks, Seq(), recursiveDistributor)
    println(s"Part1: ${result.size}")

    val lastElement = result.last

    val elements = result.zipWithIndex.filter(x => x._1 == lastElement)

    println(s"Part2: ${elements.last._2 - elements.head._2}")

  }

  benchmark {
    val result = balanceBanks(initialBanks, Seq(), optimizedDistributor)
    println(s"Part1: ${result.size}")

    val lastElement = result.last

    val elements = result.zipWithIndex.filter(x => x._1 == lastElement)

    println(s"Part2: ${elements.last._2 - elements.head._2}")

  }

}
