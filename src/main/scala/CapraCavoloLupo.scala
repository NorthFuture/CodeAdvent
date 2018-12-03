import scala.annotation.tailrec

object CapraCavoloLupo extends App with Benchmark {

  trait Element {
  }

  case object Capra extends Element

  case object Lupo extends Element

  case object Cavolo extends Element

  sealed trait Position

  case object Left extends Position

  case object Right extends Position

  case class State(position: Position, left: Set[Element], right: Set[Element]) {
    def printState(newline: Boolean = true) = {
      println(
        "[" + left.map(_.toString).mkString(",") + "]" + "-" * 10 + "[" + right.map(_.toString).mkString(",") + "]" + position
      )
      println()
    }
  }


  def calculateNewStates(state: State): Seq[State] = {

    @tailrec
    def moveElement(state: State, artifacts: Set[Element], result: Seq[State]): Seq[State] = {
      if (artifacts == Set.empty) {
        return result
      } else {
        state.position match {
          case Left => moveElement(state, artifacts.tail, result :+ State(Right, left = state.left - artifacts.head, right = state.right + artifacts.head))
          case Right => moveElement(state, artifacts.tail, result :+ State(Left, right = state.right - artifacts.head, left = state.left + artifacts.head))
        }
      }
    }

    state.position match {
      case Left => moveElement(state, state.left, Seq()) :+ State(Right, state.left, state.right)
      case Right => moveElement(state, state.right, Seq()) :+ State(Left, state.left, state.right)
    }
  }

  def isStateValid(state: State): Boolean = {
    def isValid(artifacts: Set[Element]): Boolean = {
      (!artifacts.contains(Capra) || (artifacts.contains(Capra) && !artifacts.contains(Cavolo))) &&
        (!artifacts.contains(Lupo) || (artifacts.contains(Lupo) && !artifacts.contains(Capra)))
    }

    state.position match {
      case Left => isValid(state.right)
      case Right => isValid(state.left)
    }
  }

  def isGoalState(state: State): Boolean = {
    state.left.isEmpty
  }

  @tailrec
  def solve[S](currentStates: Seq[Seq[S]], isStateValid: (S) => Boolean, isGoalState: (S) => Boolean, calculateNewStates: (S) => Seq[S]): Seq[Seq[S]] = {

    val newStates = for {
      s <- currentStates
      ns <- calculateNewStates(s.last) if (isStateValid(ns) && !s.exists(_ == ns))
    } yield (s :+ ns)

    val solution = newStates.filter(s => isGoalState(s.last))

    if (!solution.isEmpty) {
      solution
    } else {
      solve(newStates, isStateValid, isGoalState, calculateNewStates)
    }
  }

  val initialState = State(Left, Set(Capra, Cavolo, Lupo), Set())

  val solution = solve(Seq(Seq(initialState)), isStateValid, isGoalState, calculateNewStates)

  solution.foreach(x => {
    println("*" * 50);
    x.foreach(_.printState());
    println("*" * 50)
  })

  Thread.sleep(1000)
}
