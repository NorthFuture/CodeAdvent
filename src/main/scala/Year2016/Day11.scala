package Year2016

import Year2017.Benchmark

import scala.annotation.tailrec
import scala.collection.mutable

object Day11 extends App with Benchmark {

  sealed trait Container {
    val id: String
    val artifacts: Set[Artifact]

    def getGs(): Set[G] = {
      artifacts.collect { case g: G => g }
    }

    def getMs(): Set[M] = {
      artifacts.collect { case m: M => m }
    }
  }

  case class Floor(val id: String, pos: Int, artifacts: Set[Artifact]) extends Container

  case class Elevator(val id: String, floor: Floor)

  sealed trait Artifact {
    val pos: Int
    val id: String
  }

  case class G(id: String, pos: Int) extends Artifact {
    override def toString: String = id + "G"
  }

  case class M(id: String, pos: Int) extends Artifact {
    override def toString: String = id + "M"
  }

  case class State(elevator: Elevator, floors: Map[Int, Floor]) extends AbstractState {

    def moveToFloor(toFloor: Floor, a: Set[Artifact]): State = {

      val newToFloor = toFloor.copy(artifacts = toFloor.artifacts ++ a)
      val newFromFloor = elevator.floor.copy(artifacts = elevator.floor.artifacts diff a)

      val newFloors = floors - toFloor.pos - elevator.floor.pos + ((newToFloor.pos, newToFloor)) + ((newFromFloor.pos, newFromFloor))

      this.copy(elevator = this.elevator.copy(floor = newToFloor), floors = newFloors)
    }

    def printState(newline: Boolean = true): String = {
      this.floors.map(_._2).toList.sortBy(-_.pos).reverse.foldLeft("")((s: String, f: Floor) => {
        val floorArtifacts =
          f match {
            case this.elevator.floor => f.artifacts
            case x: Floor => x.artifacts
          }

        s + f.id + " " + (if (f != this.elevator.floor) ".  " else "E  ") + (1 to 10).map(x => floorArtifacts.filter(_.pos == x).headOption match {
          case None => ". "
          case Some(a: Artifact) => a.toString
        }
        ).mkString("   ") + (if (newline) "\n" else "")
      })
    }
  }

  trait AbstractState {
    def printState(newline: Boolean = true): String
  }

  @tailrec
  def solve[S <: AbstractState](currentStates: Seq[Seq[S]], seen: mutable.Set[S], isGoalState: (S) => Boolean, calculateNewStates: (S) => Seq[S]): Seq[Seq[S]] = {

    val newStates = for {
      s <- currentStates
      ns <- calculateNewStates(s.last)
      if !seen.contains(ns)
    } yield {
      seen += ns
      (s :+ ns)
    }

    println(newStates.size)

    if (newStates == Nil) {
      Nil
    } else {
      val solution = newStates.par.filter(s => isGoalState(s.last)).seq

      if (!solution.isEmpty) {
        solution
      } else {
        solve(newStates, seen, isGoalState, calculateNewStates)
      }
    }

  }

  def isGoalState(state: State): Boolean = {
    ((1 to 3) forall (state.floors(_).artifacts.isEmpty)) && state.elevator.floor.pos == 4
  }

  def isStateValid(state: State): Boolean = {

    def isThereAnyUnpoweredMs(c: Container): Boolean = {
      c.getMs().exists(m => !c.getGs().exists(g => g.id == m.id))
    }

    state.floors.forall { case (pos, x) => x.getGs().isEmpty || !isThereAnyUnpoweredMs(x) }
  }

  def calculateNewStates(state: State): List[State] = {

    @tailrec
    def recursiveCalculateNewStates(state: State, artifacts: Seq[Seq[Artifact]], result: List[State]): List[State] = {

      if (artifacts == Nil) {
        result
      } else {

        val artifactsToMove = artifacts.head

        val otherFloors = state.floors.filter { case (pos, f) => (f.pos == state.elevator.floor.pos - 1) || f.pos == state.elevator.floor.pos + 1 }

        val newResult = result ::: otherFloors.map(f => state.moveToFloor(f._2, artifactsToMove.toSet)).filter(isStateValid).toList

        recursiveCalculateNewStates(state, artifacts.tail, newResult)
      }

    }

    val allArtifacts = state.elevator.floor.artifacts.toList

    recursiveCalculateNewStates(state, allArtifacts.combinations(2).toSeq ++ allArtifacts.map(Seq(_)), List())
  }

  val floorsTest = Map(
    4 -> Floor("F4", 4, Set()),
    3 -> Floor("F3", 3, Set(G("L", 3))),
    2 -> Floor("F2", 2, Set(G("H", 1))),
    1 -> Floor("F1", 1, Set(M("H", 2), M("L", 4)))
  )

  benchmark {
    val floors = Map(
      4 -> Floor("F4", 4, Set()),
      3 -> Floor("F3", 3, Set(G("PR", 5), M("PR", 6), G("R", 7), M("R", 8))),
      2 -> Floor("F2", 2, Set(M("PL", 4), M("S", 9))),
      1 -> Floor("F1", 1, Set(G("T", 1), M("T", 2), G("PL", 3), G("S", 10)))
    )

    val initialState = State(Elevator("E", floors(1)), floors)

    val solution = solve(Seq(Seq(initialState)), collection.mutable.Set[State](initialState), isGoalState, calculateNewStates)
    println("Optimal solution found part 1:" + (solution.minBy(_.size).size - 1))
  }

  benchmark {
    val floors = Map(
      4 -> Floor("F4", 4, Set()),
      3 -> Floor("F3", 3, Set(G("PR", 5), M("PR", 6), G("R", 7), M("R", 8))),
      2 -> Floor("F2", 2, Set(M("PL", 4), M("S", 9))),
      1 -> Floor("F1", 1, Set(G("T", 1), M("T", 2), G("PL", 3), G("S", 10), G("D", 11), M("D", 11), G("E", 11), M("E", 11)))
    )

    val initialState = State(Elevator("E", floors(1)), floors)

    val solution = solve(Seq(Seq(initialState)), collection.mutable.Set[State](initialState), isGoalState, calculateNewStates)
    println("Optimal solution found part 2:" + (solution.minBy(_.size).size - 1))
  }

  Thread.sleep(1000)
}

