
package Year2015

import scala.io.Source

object Day14 extends App {

  case class Fact(a: String, b: String, gain: Int)

  val reg = """(\w*) can fly (\d*) km/s for (\d*) seconds, but then must rest for (\d*) seconds.""".r

  case class Spec(name: String, speed: Int, durationFly: Int, durationRest: Int)

  val facts = Source.fromFile("data/Year2015/day14.txt").getLines().map {
    case reg(name, speed, durationFly, durationRest) => Spec(name, speed.toInt, durationFly.toInt, durationRest.toInt)
  }.toList

  case class RunnerState(position: Int, duration: Int, isFlying: Boolean, points: Int, spec: Spec)

  val state = facts.map(x => RunnerState(0, x.durationFly, true, 0, x))

  val m = (1 to 2503).foldLeft((state, 0))((s, x) => {
    val newRunners = s._1.map(x => x.duration match {
      case 1 if x.isFlying => x.copy(x.position + x.spec.speed, x.spec.durationRest, false)
      case _ if x.isFlying => x.copy(x.position + x.spec.speed, x.duration - 1)
      case 1 if !x.isFlying => x.copy(duration = x.spec.durationFly, isFlying = true)
      case _ if !x.isFlying => x.copy(duration = x.duration - 1)
    })

    val maxPos = newRunners.maxBy(_.position).position

    val newRunnesWithPoints = newRunners.map(x => x.position match {
      case `maxPos` => x.copy(points = x.points + 1)
      case _ => x
    })

    (newRunnesWithPoints, 0)
  })._1.maxBy(_.points)

  println(m)
}