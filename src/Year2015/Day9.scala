
package Year2015

import scala.annotation.tailrec
import scala.io.Source

object Day9 extends App {

  val reg = """(\w*)\s*to\s*(\w*)\s=\s(\d*)""".r

  case class Distance(from: String, to: String, distance: Int)

  val dists = Source.fromFile("data/Year2015/tday9.txt")
    .getLines()
    .map(x => x match {
      case reg(from, to, distance) => Distance(from, to, distance.toInt)
    }).flatMap(x => Seq(x, x.copy(from = x.to, to = x.from)))
    .map(x => (x.from, x.to) -> x.distance).toMap


  case class State(currentCity: String, alreadyVisited: Seq[String], toVisit: Seq[String], path: Seq[Distance])

  @tailrec
  def solve(states: Seq[State], result: Seq[State]): Seq[State] = {

    def findNextSteps(state: State): Seq[State] = {

      state.toVisit.flatMap(x =>
        dists.get((state.currentCity, x)).map(y =>
          State(x, state.alreadyVisited :+ x, state.toVisit.filterNot(_ == x), state.path :+ Distance(state.currentCity, x, y))
        )
      )
    }

    val newStates = states.flatMap(findNextSteps).filterNot(result.contains(_))

    newStates match {
      case Nil => result ++ states
      case _ => solve(newStates, result)
    }
  }

  val cities = dists.keys.flatMap(x => x._1 :: x._2 :: Nil).toSeq

  val initialState = cities.map(x => State(x, Seq(x), cities.filterNot(_ == x), Seq()))

  val r = solve(initialState, Seq())
    .map(_.path.map(_.distance).sum)

  val (min, max) = (r.min, r.max)

  println(min)
  println(max)


}