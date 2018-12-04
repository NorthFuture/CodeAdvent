package Year2018

import java.time.{OffsetDateTime, ZoneOffset}

import scala.annotation.tailrec
import scala.io.Source

object Day4 extends App {
  val regBeingShift ="""\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] Guard #(\d+) begins shift""".r
  val regFallsAsleep = """\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] falls asleep""".r
  val regWakesUp = """\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] wakes up""".r

  trait State {
    val date: OffsetDateTime
  }


  case class BeginShift(id: Int, date: OffsetDateTime) extends State

  case class FallsAsleep(date: OffsetDateTime) extends State

  case class WakesUp(date: OffsetDateTime) extends State

  val input = Source.fromFile("data/Year2018/Day4.txt").getLines().map { x =>
    x match {
      case regBeingShift(y, m, d, h, mm, id) => BeginShift(id.toInt, OffsetDateTime.of(y.toInt, m.toInt, d.toInt, h.toInt, mm.toInt, 0, 0, ZoneOffset.UTC))
      case regFallsAsleep(y, m, d, h, mm) => FallsAsleep(OffsetDateTime.of(y.toInt, m.toInt, d.toInt, h.toInt, mm.toInt, 0, 0, ZoneOffset.UTC))
      case regWakesUp(y, m, d, h, mm) => WakesUp(OffsetDateTime.of(y.toInt, m.toInt, d.toInt, h.toInt, mm.toInt, 0, 0, ZoneOffset.UTC))

    }
  }.toList
    .sortBy(_.date)

  @tailrec
  def solveStrategy1(guardOnDuty: Int, lastTime: OffsetDateTime, input: List[State], sleepMinutes: Map[Int, Int], minutesAsleep: Map[(Int, Int), Int]): (Map[Int, Int], Map[(Int, Int), Int]) = {
    input match {
      case Nil =>
        (sleepMinutes, minutesAsleep)
      case h :: tail => h match {
        case x: BeginShift =>
          solveStrategy1(x.id, x.date, tail, sleepMinutes, minutesAsleep)
        case x: FallsAsleep =>
          solveStrategy1(guardOnDuty, x.date, tail, sleepMinutes, minutesAsleep)
        case x: WakesUp =>
          val sleepTime: Int = ((x.date.toEpochSecond - lastTime.toEpochSecond) / 60).toInt

          val existingSleepTime = sleepMinutes.getOrElse(guardOnDuty, 0) + sleepTime

          val newMinutesAsleep = (lastTime.getMinute to lastTime.getMinute + sleepTime - 1).foldLeft(minutesAsleep)((s, x) => s + (((guardOnDuty, x), s.getOrElse((guardOnDuty, x), 0) + 1)))

          solveStrategy1(guardOnDuty, x.date, tail, sleepMinutes + ((guardOnDuty, existingSleepTime)), newMinutesAsleep)
      }
    }
  }

  val state = solveStrategy1(input.head.asInstanceOf[BeginShift].id, input.head.asInstanceOf[BeginShift].date, input.tail, Map[Int, Int](), Map[(Int, Int), Int]())

  val g1 = state._1.toList.maxBy(_._2)

  val m1 = state._2.filter(_._1._1 == g1._1).maxBy(_._2)._1._2

  println(g1)
  println(g1._1 * m1)

  val g2 = state._2.toList.maxBy(_._2)

  println(g2)
  println(g2._1._1 * g2._1._2)

}

