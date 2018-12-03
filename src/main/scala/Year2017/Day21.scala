package Year2017

import scala.annotation.tailrec
import scala.io.Source

object Day21 extends App with Benchmark {

  val pattern = Matrix(List(".#.".toList, "..#".toList, "###".toList))

  case class Matrix[T](val l: List[List[T]]) {

    def apply(r: Int, c: Int): T = l(r)(c)

    def dim = l.length

    def isSame(other: Matrix[T]): Boolean = {

      val ops = List(
        (m: Matrix[T]) => m,
        (m: Matrix[T]) => m.flipH(),
        (m: Matrix[T]) => m.rotateClockwise(),
        (m: Matrix[T]) => m.rotateClockwise().rotateClockwise(),
        (m: Matrix[T]) => m.rotateClockwise().rotateClockwise().rotateClockwise(),
        (m: Matrix[T]) => m.flipH().rotateClockwise(),
        (m: Matrix[T]) => m.flipH().rotateClockwise().rotateClockwise(),
        (m: Matrix[T]) => m.flipH().rotateClockwise().rotateClockwise().rotateClockwise()
      )

      @tailrec
      def find(ops: Seq[Matrix[T] => Matrix[T]]): Boolean = {
        ops match {
          case Nil =>
            false
          case h :: tail =>
            if (h.apply(other) == this) {
              true
            } else {
              find(tail)
            }
        }
      }

      find(ops)

    }

    def split(by: Int): Matrix[Matrix[T]] = {
      Matrix(l.grouped(by).toList.map(_.map(_.grouped(by).toList).transpose)
        .map(_.map(Matrix(_))))
    }

    def flipH() = {
      Matrix(l.reverse)
    }

    def transpose() = {
      Matrix(l.transpose)
    }

    def flipV() = {
      this transpose() flipH() transpose()
    }

    def rotateAntiClockwise() = {
      transpose() flipH()
    }

    def rotateClockwise() = {
      transpose() flipV()
    }

    def printMatrix() = println(l.mkString("\n") + "\n")
  }


  val input = Source.fromFile("data/Year2017/Day21.txt").getLines()
    .map(_.split("=>"))
    .map(x => (x(0).split("/").toList, x(1).split("/").toList))
    .map(x => Matrix(x._1.map(_.trim.toList)) -> Matrix(x._2.map(_.trim.toList))).toMap


  @tailrec
  def solve(current: Matrix[Char], input: Map[Matrix[Char], Matrix[Char]], count: Int): Matrix[Char] = {
    if (count == 0) {
      current
    } else {
      val splits =
        if (current.dim % 2 == 0) {
          current.split(2)
        } else if (current.dim % 3 == 0) {
          current.split(3)
        } else {
          throw new Exception("not supported")
        }

      val x= splits.l.par.map(x => x.map(y => input.find(f => f._1.isSame(y)).map(_._2) match {
        case None => System.out.println("Not found"); y.printMatrix(); y
        case Some(x) => x
      })).toList

      val newCurrent = Matrix(x.flatMap(_.map(_.l).transpose.map(_.flatten)))

      solve(newCurrent, input, count - 1)
    }
  }

  val result = solve(pattern, input, 5)
  println(result.l.map(_.count(_ == '#')).sum)

  val result2 = solve(pattern, input, 18)
  println(result2.l.map(_.count(_ == '#')).sum)
}
