package Year2016

import scala.io.Source
import scala.util.matching.Regex

object Day12 extends App {

  val instructions = Source.fromFile("data/Year2016/Day12.txt").getLines().map(Instruction.parseInstruction)
    .toList


  println(process(instructions, State(0)))

  def process(instructions: Seq[Instruction], state: State): State = {

    if (state.instructionPointer >= instructions.length) {
      state
    } else {
      process(instructions, instructions(state.instructionPointer).execute(state))
    }
  }

  case class State(instructionPointer: Int, registries: Map[String, Int] = Map("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0))

  sealed trait Instruction {

    def execute(state: State): State
  }

  object Instruction {

    val regEx1: Regex = """cpy\s+(\d+)\s+([a-d])""".r
    val regEx2: Regex = """cpy\s+([a-d])\s+([a-d])""".r
    val regEx3: Regex = """inc\s+([a-d])""".r
    val regEx4: Regex = """dec\s+([a-d])""".r
    val regEx5: Regex = """jnz\s+(\d+)\s+(-?\d+)""".r
    val regEx6: Regex = """jnz\s+([a-d])\s+(-?\d+)""".r

    def parseInstruction(s: String): Instruction = {
      s match {
        case regEx1(v, r) => CopyValueToRegistry(v.toInt, r)
        case regEx2(rs, rd) => CopyRegistryToRegistry(rs, rd)
        case regEx3(r) => IncRegistry(r)
        case regEx4(r) => DecRegistry(r)
        case regEx5(v, s) => JumpValue(v.toInt, s.toInt)
        case regEx6(r, s) => JumpRegistry(r, s.toInt)

        case _ => Nop
      }
    }
  }

  case object Nop extends Instruction {
    override def execute(state: State): State = state
  }

  case class CopyValueToRegistry(value: Int, registry: String) extends Instruction {
    override def execute(state: State): State = state.copy(
      instructionPointer = state.instructionPointer + 1,
      registries = state.registries + ((registry, value))
    )
  }

  case class CopyRegistryToRegistry(registrySource: String, registryDest: String) extends Instruction {
    override def execute(state: State): State =
      state.registries.get(registrySource) match {
        case None => state.copy(instructionPointer = state.instructionPointer + 1)
        case Some(x) =>
          state.copy(
            instructionPointer = state.instructionPointer + 1,
            registries = state.registries + ((registryDest, x))
          )
      }
  }

  case class IncRegistry(registry: String) extends Instruction {
    override def execute(state: State): State =
      state.registries.get(registry) match {
        case None => state.copy(instructionPointer = state.instructionPointer + 1)
        case Some(x) =>
          state.copy(
            instructionPointer = state.instructionPointer + 1,
            registries = state.registries + ((registry, x + 1))
          )
      }
  }

  case class DecRegistry(registry: String) extends Instruction {
    override def execute(state: State): State =
      state.registries.get(registry) match {
        case None => state.copy(instructionPointer = state.instructionPointer + 1)
        case Some(x) =>
          state.copy(
            instructionPointer = state.instructionPointer + 1,
            registries = state.registries + ((registry, x - 1))
          )
      }
  }

  case class JumpValue(conditionValue: Int, steps: Int) extends Instruction {
    override def execute(state: State): State =
      if (conditionValue != 0)
        state.copy(instructionPointer = state.instructionPointer + steps)
      else
        state.copy(instructionPointer = state.instructionPointer + 1)
  }

  case class JumpRegistry(registry: String, steps: Int) extends Instruction {
    override def execute(state: State): State =
      state.registries.get(registry) match {
        case None => state.copy(instructionPointer = state.instructionPointer + 1)
        case Some(x) => JumpValue(x, steps).execute(state)
      }
  }

}

