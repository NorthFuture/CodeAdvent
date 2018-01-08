package Year2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day20 extends App with Benchmark {

  val regExp: Regex ="""p=<([-\d]+),([-\d]+),([-\d]+)>, v=<([-\d]+),([-\d]+),([-\d]+)>, a=<([-\d]+),([-\d]+),([-\d]+)>""".r

  val particles = Source.fromFile("data/Year2017/Day20.txt").getLines()
    .zipWithIndex
    .flatMap(x => x._1 match {
      case regExp(px, py, pz, vx, vy, vz, ax, ay, az) => Some(Particle(x._2, Vector(px.toLong, py.toLong, pz.toLong), Vector(vx.toLong, vy.toLong, vz.toLong), Vector(ax.toLong, ay.toLong, az.toLong)))
      case _ => None
    }).toList


  @tailrec
  def simulate(particles: Seq[Particle], collision: Boolean, repeatMinCheck: Long, minId: Long): Seq[Particle] = {

    val newParticles_ = particles.map(p => Particle(p.id, p.p + p.v + p.a, p.v + p.a, p.a))

    val newParticles = if (collision) {
      newParticles_.groupBy(_.p).filterNot(_._2.size > 1).flatMap(_._2).toList
    } else {
      newParticles_
    }


    val newMinId = newParticles.map(x => (x.id, x.p d())).minBy(_._2)._1

    if (newMinId == minId && repeatMinCheck > 10000) {
      particles
    } else if (newMinId == minId) {
      simulate(newParticles, collision, repeatMinCheck + 1, newMinId)
    } else {
      simulate(newParticles, collision, 0, newMinId)
    }
  }

  val result = simulate(particles, false, 0, -1)

  val min = result.map(x => (x.id, x.p d())).minBy(_._2)._1

  println(min)

  val result2 = simulate(particles, true, 0, -1)

  println(result2.size)


  case class Vector(x: Long, y: Long, z: Long) {
    def +(other: Vector) = this.copy(x = this.x + other.x, y = this.y + other.y, z = this.z + other.z)

    def d(other: Vector = Vector(0, 0, 0)) = math.abs(this.x - other.x) + math.abs(this.y - other.y) + math.abs(this.z - other.z)

  }


  case class Particle(id: Long, p: Vector, v: Vector, a: Vector)

}
