package Year2017

import scala.annotation.tailrec

object Day15 extends App with Benchmark {

  val genAFactor = 16807L
  val genBFactor = 48271L

  val divisor = 2147483647L

  val genAStart = 512L
  val genBStart = 191L

  val parisNumber = 40 * 1000 * 1000

  /*
  val genAStart = 65L
  val genBStart = 8921L
*/

  def nextValue(previousValue: Long, factor: Long, divisor: Long) = {
    (previousValue * factor) % divisor
  }

  @tailrec
  def nextValue(previousValue: Long, factor: Long, divisor: Long, multiple: Int): Long = {
    val v = (previousValue * factor) % divisor

    if (v % multiple == 0) {
      v
    }
    else {
      nextValue(v, factor, divisor, multiple)
    }
  }

  def valueMatch(a: Long, b: Long) = (a & 0xFFFF) == (b & 0xFFFF)

  val result = (1 to parisNumber).foldLeft((0L, genAStart, genBStart))((s, x) => {

    val nextAValue = nextValue(s._2, genAFactor, divisor)
    val nextBValue = nextValue(s._3, genBFactor, divisor)

    (s._1 + {
      if (valueMatch(nextAValue, nextBValue)) 1 else 0
    }, nextAValue, nextBValue)
  })

  print("Count: " + result._1)

  def solvePart2(currentAValue: Long, currentBValue: Long): Int = {

    @tailrec
    def internalSolve(count: Int, matchCount: Int, currentAValue: Long, currentBValue: Long): Int = {

      if (count == 5 * 1000 * 1000) {
        matchCount
      } else {
        val nextAValue = nextValue(currentAValue, genAFactor, divisor, 4)
        val nextBValue = nextValue(currentBValue, genBFactor, divisor, 8)


        if (valueMatch(nextAValue, nextBValue)) {
          internalSolve(count + 1, matchCount + 1, nextAValue, nextBValue)
        } else {
          internalSolve(count + 1, matchCount, nextAValue, nextBValue)
        }
      }
    }

    internalSolve(0, 0, genAStart, genBStart)
  }

  val r = solvePart2(genAStart, genBStart)

  println(r)

}
