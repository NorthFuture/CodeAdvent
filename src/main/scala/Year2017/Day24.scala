package Year2017

import scala.annotation.tailrec
import scala.io.Source

object Day24 extends App with Benchmark {

  val input: Seq[Component] = Source.fromFile("data/Year2017/Day24.txt").getLines()
    .map(_.split("/").toList match { case h :: t :: Nil => Component(h.toInt, t.toInt) }).toList


  sealed trait Port {

  }

  case object BothPort extends Port

  case object PortA extends Port

  case object PortB extends Port

  case class Component(a: Int, b: Int, freePort: Port = BothPort) {
    def strength: Int = a + b

    def isSameComponent(other: Component): Boolean = a == other.a && b == other.b

    def getFreePins: Int = freePort match {
      case PortA => a
      case PortB => b
      case _ => 0
    }
  }

  case class Bridge(items: Seq[Component]) {
    def append(c: Component) = this.copy(items = items :+ c)

    def print = println(items.map(x => x.a + "/" + x.b).mkString("--"))
  }

  def solve(components: Seq[Component], bridges: Seq[Bridge]): Seq[Bridge] = {

    def extendBridge(bridge: Bridge): Seq[Bridge] = {
      findCompatibleComponents(bridge.items.last.getFreePins, components)
        .flatMap(x => solve(components.filterNot(_.isSameComponent(x)), Seq(bridge.append(x))))
    }

    if (components.isEmpty) {
      bridges
    } else {
      bridges ++ bridges.flatMap(extendBridge)
    }

  }


  def findCompatibleComponents(pins: Int, components: Seq[Component]): Seq[Component] = {

    @tailrec
    def internal(components: Seq[Component], compatibles: Seq[Component]): Seq[Component] = {

      components match {
        case Nil =>
          compatibles
        case h :: tail if h.a == pins =>
          internal(tail, compatibles :+ h.copy(freePort = PortB))
        case h :: tail if h.b == pins =>
          internal(tail, compatibles :+ h.copy(freePort = PortA))
        case h :: tail =>
          internal(tail, compatibles)
      }
    }

    internal(components, Seq())
  }

  val components0 = findCompatibleComponents(0, input)

  val result = solve(input.filterNot(x => components0.exists(_.isSameComponent(x))), components0.map(x => Bridge(Seq(x))))

  val resultWithMetrics = result.map(x => (x, x.items.foldLeft(0)(_ + _.strength)))

  val maxByStrength = resultWithMetrics.maxBy(_._2)

  println(maxByStrength._2)

  val max2 = resultWithMetrics.maxBy(x => (x._1.items.length, x._2))

  println(max2._2)


}
