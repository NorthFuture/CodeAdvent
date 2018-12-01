package Year2018

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day1 extends App {

  def getInput() = Source.fromFile("data/Year2018/Day1.txt").getLines().map(_.toInt)

  def solvePart1(input: Stream[Int]) = input.sum

  def solvePart2(input: Stream[Int]) = {

    @tailrec
    def solve(input: Stream[Int], currentFreq: Int, seenFreqs: mutable.Set[Int]): Option[Int] = {

      input match {
        case h #:: tail =>
          val newFreq = currentFreq + h

          if (seenFreqs.contains((newFreq))) {
            Some(newFreq)
          } else {
            solve(tail, newFreq, seenFreqs += newFreq)
          }
        case Stream() => None
      }

    }

    solve(input, 0, mutable.Set())
  }

  def streamFromIterator(inputGetter: () => Iterator[Int]): Stream[Int] = {
    val input = inputGetter()

    def getS(): Stream[Int] = if (input.hasNext) input.next() #:: getS() else Stream.empty[Int]

    getS()
  }

  def recursiveStream(inputGetter: () => Iterator[Int]): Stream[Int] = {

    streamFromIterator(inputGetter) #::: recursiveStream(inputGetter)
  }

  println(solvePart1(streamFromIterator(getInput)))

  println(solvePart2(recursiveStream(getInput)))
}

