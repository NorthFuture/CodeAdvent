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

  case class Guard(id: Int, totalSleepTime: Int = 0, sleepTimeByMinutes: Map[Int, Int] = Map()) {
    def sleep(minute: Int): Guard = {
      this.copy(sleepTimeByMinutes = sleepTimeByMinutes + ((minute, sleepTimeByMinutes.getOrElse(minute, 0) + 1)))
    }

    def sleep(fromMinute: Int, toMinute: Int): Guard = {
      (fromMinute to toMinute).foldLeft(this)((s, m) => s.sleep(m)).copy(totalSleepTime = totalSleepTime + toMinute - fromMinute)
    }
  }

  val input = Source.fromFile("data/Year2018/Day4.txt").getLines().map { x =>
    x match {
      case regBeingShift(y, m, d, h, mm, id) => BeginShift(id.toInt, OffsetDateTime.of(y.toInt, m.toInt, d.toInt, h.toInt, mm.toInt, 0, 0, ZoneOffset.UTC))
      case regFallsAsleep(y, m, d, h, mm) => FallsAsleep(OffsetDateTime.of(y.toInt, m.toInt, d.toInt, h.toInt, mm.toInt, 0, 0, ZoneOffset.UTC))
      case regWakesUp(y, m, d, h, mm) => WakesUp(OffsetDateTime.of(y.toInt, m.toInt, d.toInt, h.toInt, mm.toInt, 0, 0, ZoneOffset.UTC))
    }
  }.toList
    .sortBy(_.date)

  def getOrCreateGuard(guardId: Int)(implicit guards: Set[Guard]): Guard = {

    guards.find(_.id == guardId) match {
      case None => Guard(guardId)
      case Some(x) => x
    }
  }

  @tailrec
  def solveStrategy1(guardOnDuty: Guard, lastTime: OffsetDateTime, input: List[State])(implicit guards: Set[Guard]): Set[Guard] = {
    input match {
      case Nil =>
        guards
      case h :: tail => h match {
        case x: BeginShift =>
          solveStrategy1(getOrCreateGuard(x.id), x.date, tail)(guards)
        case x: FallsAsleep =>
          solveStrategy1(guardOnDuty, x.date, tail)(guards)
        case x: WakesUp =>
          val sleepTime: Int = ((x.date.toEpochSecond - lastTime.toEpochSecond) / 60).toInt

          val newGuardOnDuty = guardOnDuty.sleep(lastTime.getMinute, lastTime.getMinute + sleepTime - 1)

          solveStrategy1(newGuardOnDuty, x.date, tail)(guards + (newGuardOnDuty))
      }
    }
  }

  implicit val initialState = Set[Guard]()

  val finalState = solveStrategy1(getOrCreateGuard(input.head.asInstanceOf[BeginShift].id), input.head.asInstanceOf[BeginShift].date, input.tail)

  val g1 = finalState.maxBy(_.totalSleepTime)

  println(g1)
  println(g1.id * g1.sleepTimeByMinutes.maxBy(_._2)._2)

  val g2 = finalState.maxBy(x => x.sleepTimeByMinutes.maxBy(_._2))

  println(g2)
  println(g2.id * g2.sleepTimeByMinutes.maxBy(_._2)._2)

}

