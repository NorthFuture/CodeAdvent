package Year2018

import scala.annotation.tailrec
import scala.io.Source

object Day7 extends App {

  val reg ="""Step (\w) must be finished before step (\w) can begin.""".r

  val input = Source.fromFile("data/Year2018/Day7.txt").getLines().map {
    case reg(before, after) => (before.head, after.head)
  }.toList

  val plan = input.foldLeft(Map[Step, Seq[Step]]())((s, x) => s + ((x._1, s.getOrElse(x._1, Seq()) :+ x._2)))

  type Step = Char

  trait ProcessResult {

  }

  case class Start(step: Step) extends ProcessResult

  case class Complete(step: Step) extends ProcessResult

  case class WorkerComplete(step: Step) extends ProcessResult

  case object Waiting extends ProcessResult

  case object FullyCompleted extends ProcessResult

  trait State[S <: State[S]] {
    def process(): (ProcessResult, S)
  }

  case class Part1Resolver(doable: Set[Step], steps: Map[Step, Seq[Step]], stepsDone: Seq[Step]) extends State[Part1Resolver] {
    override def process(): (ProcessResult, Part1Resolver) = {

      if (doable.isEmpty && steps.isEmpty) { // fully resolved
        (FullyCompleted, this)
      } else {
        val newDoable = steps.filter(x => !steps.exists(y => y._2.contains(x._1))).keys.toSet ++ doable

        val nextStep = newDoable.minBy(x => x)

        val newSteps = steps.filterNot(_._1 == nextStep)

        if (newSteps.isEmpty) { // no more deps
          (Complete(nextStep), Part1Resolver(newDoable - nextStep ++ steps.flatMap(_._2), newSteps, stepsDone :+ nextStep))
        } else {
          (Complete(nextStep), Part1Resolver(newDoable - nextStep, newSteps, stepsDone :+ nextStep))
        }
      }
    }
  }

  case class Part2Resolver(workers: Set[Worker], steps: Map[Step, Seq[Step]], readyForWorker: Set[Step], workerCompleted: Set[Step], fullyCompletedSteps: Seq[Step], time: Int) extends State[Part2Resolver] {
    override def process(): (ProcessResult, Part2Resolver) = {
      if (readyForWorker.isEmpty && steps.isEmpty && workerCompleted.isEmpty && workers.forall(_.idle)) { // fully resolved
        (FullyCompleted, this)
      } else if (workerCompleted.nonEmpty) {
        val nextStep = workerCompleted.minBy(x => x)
        val newSteps = steps - nextStep

        if (newSteps.isEmpty) { // Last step
          (Complete(nextStep), copy(workerCompleted = workerCompleted - nextStep, steps = newSteps, readyForWorker = (readyForWorker - nextStep) ++ steps.flatMap(_._2), fullyCompletedSteps = (fullyCompletedSteps :+ nextStep)))
        } else {
          (Complete(nextStep), copy(workerCompleted = workerCompleted - nextStep, steps = newSteps, fullyCompletedSteps = fullyCompletedSteps :+ nextStep))
        }
      } else {
        val newDoable = (steps.filter(x => !steps.exists(y => y._2.contains(x._1))).keys.toSet ++ readyForWorker).filterNot(x => workers.exists(w => w.workingStep.contains(x)))

        val nextStep = if (newDoable.isEmpty) None else Some(newDoable.minBy(x => x))

        val idleWorker = workers.find(_.idle)

        (idleWorker, nextStep) match {
          case (Some(w), Some(s)) =>
            val nw = w.start(s)

            val ww = workers - w
            (Start(s), copy(workers = ww + nw))

          case (None, None) | (None, Some(_)) | (Some(_), None) =>
            val newWorkers = workers.map(_.tick())

            (Waiting, copy(workers = newWorkers.map(x => x._1), time = time + 1, workerCompleted = workerCompleted ++ newWorkers.flatMap(_._2)))
        }
      }
    }
  }

  @tailrec
  def solve[S <: State[S]](state: S): S = {

    val (result, newState) = state.process()

    result match {
      case FullyCompleted =>
        newState

      case _ =>
        solve(newState)
    }
  }

  val result1 = solve(Part1Resolver(Set(), plan, Seq()))

  //  println(result1.stepsDone.mkString(""))
  //  println(result1.stepsDone.mkString("") == "CABDFE")

  val workers = (0 to 4).map(Worker(_)).toSet

  val result2 = solve(new Part2Resolver(workers, plan, Set(), Set(), Seq(), 0))

  println(result2.fullyCompletedSteps.mkString(""))
  println(result2.fullyCompletedSteps.mkString("") == "CABFDE")
  println(result2.time)

  case class Worker(id: Int, idle: Boolean = true, workingStep: Option[Step] = None, timeLeft: Int = 0) {

    override def equals(that: Any): Boolean =
      that match {
        case that: Worker => that.id == this.id
        case _ => false
      }

    override def hashCode: Int = {
      id.hashCode()
    }

    def start(step: Step) = {
      copy(idle = false, workingStep = Some(step), timeLeft = 60 + step - 'A')
    }

    def tick(): (Worker, Option[Step]) = {
      if (idle) {
        (this, None)
      } else if (timeLeft == 0) {
        (copy(idle = true, workingStep = None), workingStep)
      } else {
        (copy(timeLeft = timeLeft - 1), None)
      }
    }
  }

}
