package Year2019

import scala.io.Source

object Day2 extends App {

  def getInput(): Seq[Array[Int]] =
    Source
      .fromFile("data/Year2019/Day2.txt")
      .getLines()
      .map(_.split(",").map(_.toInt))
      .toSeq

  def iterate(pos: Int, data: Array[Int]): Either[String, Array[Int]] = {
    data(pos) match {
      case 1 =>
        data(data(pos + 3)) = data(data(pos + 1)) + data(data(pos + 2))
        iterate(pos + 4, data)
      case 2 =>
        data(data(pos + 3)) = data(data(pos + 1)) * data(data(pos + 2))
        iterate(pos + 4, data)
      case 99 => Right(data)
      case x  => Left(s"Wrong opcode $x")
    }

  }

  def part1() = {
    getInput()
      .map(iterate(0, _))
      .map(_.map(_.mkString(",")))
      .foreach(println)
  }

  def part2(d: Array[Int], noun: Int, verb: Int): Boolean = {
    val data = Array(d: _*)
    data(1) = noun
    data(2) = verb

    iterate(0, data).exists(x => x(0) == 19690720)
  }

  part1()

  val resultPart2 = for {
    data <- getInput()
    noun <- 0 to 99
    verb <- 0 to 99 if part2(data, noun, verb)
  } yield (data, noun, verb)

  resultPart2.map(x => x._2 * 100 + x._3).foreach(println)
}
