package Year2017

import java.io.{FileInputStream, ObjectInputStream}

import scala.annotation.tailrec
import scala.collection.mutable

object Day14 extends App with Benchmark {

  class ObjectInputStreamWithCustomClassLoader(
                                                fileInputStream: FileInputStream
                                              ) extends ObjectInputStream(fileInputStream) {
    override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
      try {
        Class.forName(desc.getName, false, getClass.getClassLoader)
      }
      catch {
        case ex: ClassNotFoundException => super.resolveClass(desc)
      }
    }
  }

  val gridSize = 128

  /*
      val input = "flqrgnkx"; //stpzcrnm"

    def knotHash(s: String): Seq[Boolean] = {

      import Day10.{State, process}

      val steps2 = s.filter(_ != ' ').map(_.toInt) ++: Seq(17, 31, 73, 47, 23)

      val x = (0 to 63).flatMap(x => steps2)
        .foldLeft(State(0 to 255 toList, 0, 0))(process)
        .list.grouped(16).map(x => x.tail.foldLeft(x.head)((s, n) => s ^ n)).toList

      //  println(x.map(_.formatted("%02x")).mkString(""))

      x.flatMap(x => (7 to 0 by -1)
        .map(i => if (((x >> i) & 1) != 0) true else false))
    }

    def mapRow(s: Seq[Boolean]): String = {
      s.map(x => if (x) "#" else ".").mkString("")
    }

    val matrix: Map[(Int, Int), Boolean] = (for {
      r <- 0 to 127
      c <- knotHash(input + "-" + r).zipWithIndex
    } yield ((r, c._2) -> c._1)).toMap


  */
  val fis = new FileInputStream("c:\\file.txt")
  val ois = new ObjectInputStreamWithCustomClassLoader(fis)

  val matrix = ois.readObject.asInstanceOf[Map[(Int, Int), Boolean]]

  val usedSpace = matrix.count(x => x._2)

  def markRegions(gridSize: Int, matrix: Map[(Int, Int), Boolean]): Seq[((Int, Int), Int)] = {

    val mutableMatrix = mutable.Map(matrix.map(x => if (x._2) x._1 -> Some(0) else x._1 -> None).toSeq: _*)

    @tailrec
    def findUnmarkedBlock(r: Int, c: Int, matrix: Map[(Int, Int), Boolean]): Option[(Int, Int)] = {
      (r, c) match {
        case (`matrixSize`, _) =>
          None
        case (x, `matrixSize`) =>
          findUnmarkedBlock(r + 1, 0, matrix)
        case (r, c) if matrix((r, c)) =>
          Some((r, c))
        case (r, c) =>
          findUnmarkedBlock(r, c + 1, matrix)
      }
    }

    def freeSpace(pos: Seq[(Int, Int)], matrix: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] = {
      pos.foldLeft(matrix)((m, x) => m + ((x, false)))
    }

    @tailrec
    def markRegion(regionNumber: Int, blocks: Seq[(Int, Int)]) = {




      blocks.foreach(p => mutableMatrix.update(p, None)
      val newMatrix = freeSpace(toInspect, matrix)

      val newToInspect = toInspect.foldLeft(Seq[(Int, Int)]())((s, x) => {
        val otherCells = Seq((x._1, x._2 + 1), (x._1, x._2 - 1), (x._1 + 1, x._2), (x._1 - 1, x._2))

        s ++ (otherCells.map(x => (x, newMatrix.get(x))) collect {
          case (pos: (Int, Int), Some(true)) if !s.exists(_ == pos) => pos
        })
      }
      )

      val newResult = result ++ toInspect.map(x => (x, regionNumber))

      if (newToInspect.isEmpty) {
        findUnmarkedBlock(0, 0, newMatrix) match {
          case None =>
            result ++ toInspect.map(x => (x, regionNumber)) // it's over
          case Some(x) =>
            markRegion(regionNumber + 1, newMatrix, Seq(x), newResult)
        }
      } else {
        markRegion(regionNumber, freeSpace(newToInspect, newMatrix), newToInspect, newResult ++ newToInspect.map(x => (x, regionNumber)))
      }
    }

    val initialPos = (0, 0)

    markRegion(1, initialPos)
  }

  println("Used space:" + usedSpace)
  /*
    println((0 to 20 map { r =>
      (0 to 20 map { c =>
        matrix.find(x => x._1 == (r, c)) match {
          case None => "."
          case Some(x) => if (x._2) "#" else "."
        }
      }).mkString("")
    }).mkString("\n"))
  */
  val regionsMatrixCount = markRegions(gridSize, matrix)

  println("Regions count:" + regionsMatrixCount.map(_._2).distinct.size)
  /*
    val r = (0 to 20 map { r =>
      (0 to 20 map { c =>
        regionsMatrixCount.find(_._1 == (r, c)) match {
          case None =>
            ".. "
          case Some(x) =>
            x._2.formatted("%02d") + " "
        }
      }).mkString("")
    }).mkString("\n")

  println(r)
  */
}
