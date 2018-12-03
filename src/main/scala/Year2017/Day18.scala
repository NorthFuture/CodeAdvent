package Year2017

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.Try

object Day18 extends App with Benchmark {

  val instructions = Source.fromFile("data/Year2017/Day16.txt").getLines().map(parseInstruction).toList

  @tailrec
  def run(instructions: Seq[Instruction], state: State): State = {
    val x = instructions.lift(state.ip)

    x match {
      case None =>
        state
      case Some(x) =>

        val newState = x.execute(state)

        if (newState.terminated || newState.suspended) {
          newState
        } else {
          run(instructions, newState)
        }
    }
  }

  @tailrec
  def run2(instructions: Seq[Instruction], states: (State, State)): (State, State) = {
    val step_0 = (run(instructions, states._1), run(instructions, states._2))

    println(states._1)
    println(states._2)

    @tailrec
    def transferQueue(from: State, to: State): (State, State) = {
      from.outQueue.dequeueOption match {
        case None => (from, to)
        case Some(x) =>
          println(s"moving ${x} from ${from.id} to ${to.id}")
          transferQueue(from.copy(outQueue = x._2), to.copy(inQueue = to.inQueue.enqueue(x._1)))
      }
    }

    val step_1 = transferQueue(step_0._1, step_0._2)
    val step_2 = transferQueue(step_1._2, step_1._1) // reversed order

    val newState = (step_2._2, step_2._1)
    if (newState == states) {
      newState
    } else {
      run2(instructions, newState)
    }
  }

  val result = run(instructions, State(Map(), 0, 0, true, false, false, Queue(), Queue()))

  println(result.regs(Snd.LAST_SOUND))

  val result2 = run2(instructions,
    (State(Map("p" -> 0), 0, 0, false, false, false, Queue(), Queue()),
      State(Map("p" -> 1), 1, 0, false, false, false, Queue(), Queue()))
  )

  println(result2._2.regs(Snd.COUNT))

  def parseInstruction(s: String): Instruction = {

    val regExp1 ="""(snd|rcv)\s+(\w+|\d+)""".r

    val regExp2 ="""(set|add|mul|mod|jgz)\s+(\w+|\d+)\s+(\w+|[-\d]+)""".r

    s match {
      case regExp1(a, b) if a == "snd" => Snd(b)
      case regExp1(a, b) if a == "rcv" => Rcv(b)
      case regExp2(a, b, c) if a == "set" => Set(b, c)
      case regExp2(a, b, c) if a == "add" => Add(b, c)
      case regExp2(a, b, c) if a == "mul" => Mul(b, c)
      case regExp2(a, b, c) if a == "mod" => Mod(b, c)
      case regExp2(a, b, c) if a == "jgz" => Jump(b, c)
      case _ => Nop
    }
  }

  case class State(regs: Map[String, Long], id: Int, ip: Int, soundMode: Boolean, suspended: Boolean, terminated: Boolean, inQueue: Queue[Long], outQueue: Queue[Long])

  trait Instruction {
    def execute(implicit s: State): State

    def getValueOrRegistry(x: String)(implicit s: State) = {
      Try(x.toLong).toOption.getOrElse(s.regs.get(x).getOrElse(0L))
    }
  }

  trait AutoIncrementIpInstruction extends Instruction {
    override def execute(implicit s: State): State = {
      val newState = internalExecute(s)
      newState.copy(ip = newState.ip + 1)
    }

    def internalExecute(implicit s: State): State
  }

  case object Nop extends Instruction {
    override def execute(implicit s: State): State = s
  }

  case class Snd(x: String) extends AutoIncrementIpInstruction {
    override def internalExecute(implicit s: State): State =
      if (s.soundMode) {
        s.copy(regs = s.regs + ((Snd.LAST_SOUND, getValueOrRegistry(x))))
      } else {
        println(s"Sending ${getValueOrRegistry(x)} from ${s.id}")
        s.copy(outQueue = s.outQueue.enqueue(getValueOrRegistry(x)), regs = s.regs + ((Snd.COUNT, getValueOrRegistry(Snd.COUNT) + 1)))
      }
  }

  case object Snd {
    val LAST_SOUND = "lastsound"
    val COUNT = "count"
  }

  case class Set(reg: String, value: String) extends AutoIncrementIpInstruction {
    override def internalExecute(implicit s: State): State = s.copy(regs = s.regs + ((reg, getValueOrRegistry(value))))
  }

  trait AritmeticOperation extends AutoIncrementIpInstruction {
    val reg: String
    val value: String

    def op(a: Long, b: Long): Long

    override def internalExecute(implicit s: State): State = s.copy(regs = s.regs + ((reg, op(getValueOrRegistry(reg), getValueOrRegistry(value)))))
  }

  case class Add(reg: String, value: String) extends AritmeticOperation {
    def op(a: Long, b: Long): Long = a + b
  }

  case class Mul(reg: String, value: String) extends AritmeticOperation {
    def op(a: Long, b: Long): Long = a * b
  }

  case class Mod(reg: String, value: String) extends AritmeticOperation {
    def op(a: Long, b: Long): Long = a % b
  }

  case class Rcv(value: String) extends Instruction {
    override def execute(implicit s: State): State =
      if (s.soundMode) {
        if (getValueOrRegistry(value) > 0) s.copy(terminated = true) else s.copy(ip = s.ip + 1)
      } else {
        s.inQueue.dequeueOption match {
          case None => println(s"Receiving ${s.id} into ${value} suspend"); s.copy(suspended = true)
          case Some(x) => println(s"Receiving from ${s.id} into ${value}"); s.copy(inQueue = x._2, suspended = false, regs = s.regs + ((value, x._1)))
        }
      }
  }

  case class Jump(value: String, offset: String) extends Instruction {
    override def execute(implicit s: State): State = s.copy(ip = s.ip + (if (getValueOrRegistry(value) > 0) getValueOrRegistry(offset).toInt else 1))
  }

}
