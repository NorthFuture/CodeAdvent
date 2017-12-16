package Year2017

import scala.collection.Seq

object Day10S extends App with Benchmark {


  val input = (4, Seq(3, 4, 1, 5))
  val _input = (255, Seq(34, 88, 2, 222, 254, 93, 150, 0, 199, 255, 39, 32, 137, 136, 1, 167))

  case class State(list: CircularList[Int], currentPosition: Int, skip: Int)

  class CircularList[A](seq: Seq[A]) {
    lazy val internalStream: Stream[A] = seq.toStream #::: internalStream

    def length: Int = seq.length

    def zipWithIndex(): Seq[(Int, A)] = {
      (0 to length).zip(internalStream.take(length))
    }
  }

  val steps = input._2.scanLeft(State(new CircularList(0 to input._1), 0, 0))((s, e) => {
    val reversedList = s.list.internalStream.drop(s.currentPosition).take(e).toList.reverse
    val tail = s.list.internalStream.drop((s.currentPosition + e) % s.list.length).take(s.list.length - reversedList.length).toList

    State(new CircularList(reversedList ++ tail), (s.currentPosition + s.skip + e) % s.list.length, s.skip + 1)
  })


  steps.map(s => s.list.zipWithIndex.map(x => if (x._1 == s.currentPosition) "(" + x._2 + ")" else " " + x._2.toString + " ").mkString(" "))
    .foreach(println)


  val resultList = steps.last.list.internalStream.take(2)

  println(resultList(0) * resultList(1))
}

