package Year2017

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.Try

object Day23 extends App with Benchmark {

  val instructions = Source.fromFile("data/Year2017/Day23.txt").getLines().map(parseInstruction).toList

  @tailrec
  def run(instructions: Seq[Instruction], state: State): State = {
    val x = instructions.lift(state.ip)

    x match {
      case None =>
        state
      case Some(x) =>
        val profileKey = "count_" + x.getClass.getSimpleName.toString
        //    println(x + "\t\t" + state.regs)

        val newState_ = x.execute(state)

        val newState = newState_.copy(regs = newState_.regs + ((profileKey, newState_.regs.get(profileKey).getOrElse(0L) + 1L)))

        if (newState.terminated || newState.suspended) {
          newState
        } else {
          run(instructions, newState)
        }
    }
  }


  val result = run(instructions, State(Map("a" -> 0L, "h" -> 0L), 0, 0, true, false, false, Queue(), Queue()))

  println(result.regs("count_Mul"))

  def parseInstruction(s: String): Instruction = {

    val regExp2 ="""(set|sub|mul|jnz)\s+(\w+|\d+)\s+(\w+|[-\d]+)""".r

    s match {
      case regExp2(a, b, c) if a == "set" => Set(b, c)
      case regExp2(a, b, c) if a == "sub" => Sub(b, c)
      case regExp2(a, b, c) if a == "mul" => Mul(b, c)
      case regExp2(a, b, c) if a == "jnz" => Jump(b, c)
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

  case class Set(reg: String, value: String) extends AutoIncrementIpInstruction {
    override def internalExecute(implicit s: State): State = s.copy(regs = s.regs + ((reg, getValueOrRegistry(value))))
  }

  trait AritmeticOperation extends AutoIncrementIpInstruction {
    val reg: String
    val value: String

    def op(a: Long, b: Long): Long

    override def internalExecute(implicit s: State): State = s.copy(regs = s.regs + ((reg, op(getValueOrRegistry(reg), getValueOrRegistry(value)))))
  }


  case class Sub(reg: String, value: String) extends AritmeticOperation {
    def op(a: Long, b: Long): Long = a - b
  }

  case class Mul(reg: String, value: String) extends AritmeticOperation {
    def op(a: Long, b: Long): Long = a * b
  }


  case class Jump(value: String, offset: String) extends Instruction {
    override def execute(implicit s: State): State = s.copy(ip = s.ip + (if (getValueOrRegistry(value) != 0) getValueOrRegistry(offset).toInt else 1))
  }

}
