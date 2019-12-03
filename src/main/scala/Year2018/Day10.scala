package Year2018


import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.matching.Regex

object Day10 extends App {
  val reg: Regex = """\s*position=<\s*([-\d]*),\s*([-\d]*)> velocity=<\s*([-\d]*),\s*([-\d]*)>\s*""".r

  case class Particle(position: (Long, Long), velocity: (Int, Int))

  val input = Day10Input.input.split("\r\n")
    .map { case reg(px, py, vx, vy) => Particle((px.toInt, py.toInt), (vx.toInt, vy.toInt)) }
    .toSeq.map(tickParticle(0))

  def tickParticle(time: Int)(particle: Particle): Particle = {
    particle.copy(
      position = (particle.position._1 + particle.velocity._1 * time, (particle.position._2 + particle.velocity._2 * time)))
  }

  def tick(s: Seq[Particle]): Seq[Particle] = {
    s.map(tickParticle(1))
  }

  @tailrec
  def solve(s: Seq[Particle], count: Int, diffusionFactor: Long): (Int, Seq[Particle]) = {
    val t = tick(s)

    val center_sum = t.foldLeft((0L, 0L))((s, x) => (s._1 + x.position._2, s._2 + x.position._1))


    val xm = center_sum._1 / t.length
    val ym = center_sum._2 / t.length

    val newDiffusionFactor = t.map(x => Math.abs(x.position._2 - xm) + Math.abs(x.position._1 - ym)).sum

    if (newDiffusionFactor < diffusionFactor) {
      solve(t, count + 1, newDiffusionFactor)
    } else {
      (count, s)
    }
  }

  val result = solve(input, 0, Long.MaxValue)

  /*
    def delayedFuture[T](millis: Double, param: T = Unit): Future[T] = {
      val p = Promise[T]()

      setTimeout(millis) {
        println(s"completing ${millis / 1000}")
        p.success(param)
      }

      p.future
    }

    def printCanvas(evolution: Seq[(Int, Seq[Particle], Long)]): Unit = {
      implicit val ec = ExecutionContext.global

      val canvasSize = (1200, 900)
      val canvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
      canvas.width = canvasSize._1
      canvas.height = canvasSize._2

      val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
      dom.document.body.appendChild(canvas)

      def printOnCanvas(p: (Long, Long)) = {
        ctx.fillRect(p._1, p._2, 2, 2)
      }

      def reframe(max: (Long, Long), canvasSize: (Int, Int))(p: Particle): (Long, Long) = {
        (Math.round(1.0 * p.position._1 / max._1 * canvasSize._1 / 2) + canvasSize._1 / 2, Math.round(1.0 * p.position._2 / max._2 * canvasSize._2 / 2) + canvasSize._2 / 2)
      }

      def paintParticles(s: Seq[Particle]) = {
        val xx = s.map(_.position._1)
        val yy = s.map(_.position._2)
        val miny = xx.min
        val maxy = xx.max

        val minx = yy.min
        val maxx = yy.max

        val maxX = Math.abs(maxx) + Math.abs(minx)
        val maxY = Math.abs(maxy) + Math.abs(miny)

        println((maxX, maxY))
        ctx.fillStyle = "#d3d3d3"
        ctx.fillRect(0, 0, canvas.width, canvas.height)
        ctx.fillStyle = "black"
        s.map(reframe((maxX, maxY), canvasSize)).foreach(printOnCanvas)
      }

      evolution.map(x => delayedFuture(x._1 * 50, x._2))
        .foreach(_.foreach(paintParticles))
    }
  */

  def mapToText(data: Seq[Particle]): String = {

    def buildCanvas(m: Set[(Long, Long)]): String = {
      val miny = m.minBy(_._1)._1
      val maxy = m.maxBy(_._1)._1

      val minx = m.minBy(_._2)._2
      val maxx = m.maxBy(_._2)._2

      (minx to maxx).map { x =>
        (miny to maxy).map { y =>
          if (m((y, x))) "#" else " "
        }.mkString("")
      }.mkString("\n")
    }

    buildCanvas(data.foldLeft(Set[(Long, Long)]())((s, x) => s + ((x.position))))
  }

  println(mapToText(result._2))
  println(result._1)

}

