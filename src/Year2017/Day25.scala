package Year2017

import scala.annotation.tailrec
import scala.io.Source

object Day25 extends App with Benchmark {


  sealed trait State {}

  case object StateA extends State

  case object StateB extends State

  case object StateC extends State

  case object StateD extends State

  case object StateE extends State

  case object StateF extends State

  case object Error extends State

  case class MachineInstruction(value: Boolean, positionOffset: Long, nextState: State)

  case class MachineStateInstruction(state: State, instructionTrue: MachineInstruction, instructionFalse: MachineInstruction)

  case class MachineInstructions(instructions: Map[State, MachineStateInstruction] = Map())

  case class MachineState(state: State, position: Long, values: Map[Long, Boolean]) {
    def readValue = values.get(position).getOrElse(false)

    def move(positionOffset: Long) = this.copy(position = position + positionOffset)

    def writeValue(value: Boolean) = this.copy(values = values + ((position, value)))

    def setState(state: State) = copy(state = state)
  }

  case class TuringMachine(machineInstructions: MachineInstructions, machineState: MachineState) {
    def setState(state: State) = copy(machineState = machineState.setState(state))
  }

  @tailrec
  def process(turingMachine: TuringMachine, count: Long): TuringMachine = {

    if (count == 0) {
      turingMachine
    } else {

      turingMachine.machineInstructions.instructions.get(turingMachine.machineState.state) match {
        case None =>
          turingMachine.setState(state = Error)
        case Some(x) =>
          val instruction = if (turingMachine.machineState.readValue) x.instructionTrue else x.instructionFalse

          val newMachineState = turingMachine.machineState.writeValue(instruction.value).move(instruction.positionOffset)
            .setState(instruction.nextState)

          process(turingMachine.copy(machineState = newMachineState), count - 1)
      }
    }
  }

  val input = Source.fromFile("data/Year2017/Day25.txt").getLines().mkString("\n")


  def parseInstructions(input: String): (TuringMachine, Long) = {

    val regBeginstate ="""Begin in state ([A-Z]).""".r

    val regChecksum = """Perform a diagnostic checksum after (\d+) steps.""".r

    val regexpState =
      """(?s)In state ([A-Z]):\s*
  If the current value is 0:
    - Write the value (0|1).
    - Move one slot to the (right|left).
    - Continue with state ([A-Z]).
  If the current value is 1:
    - Write the value (0|1).
    - Move one slot to the (right|left).
    - Continue with state ([A-Z]).""".r

    def parseState(state: String): State = state match {
      case "A" => StateA
      case "B" => StateB
      case "C" => StateC
      case "D" => StateD
      case "E" => StateE
      case "F" => StateF
    }

    def parseMovement(mov: String): Long = mov match {
      case "left" => -1L
      case "right" => 1L
    }

    def parseBoolean(x: String): Boolean = x match {
      case "0" => false
      case "1" => true
    }

    val initialState = parseState(regBeginstate.findAllMatchIn(input).toSeq.head.group(1))
    val checkSum = regChecksum.findAllMatchIn(input).toSeq.head.group(1).toLong

    val machineInstructions = regexpState.findAllMatchIn(input).map(x =>
      parseState(x.group(1)) -> MachineStateInstruction(
        parseState(x.group(1)),
        MachineInstruction(parseBoolean(x.group(5)), parseMovement(x.group(6)), parseState(x.group(7))),
        MachineInstruction(parseBoolean(x.group(2)), parseMovement(x.group(3)), parseState(x.group(4)))
      )).toMap


    (TuringMachine(MachineInstructions(machineInstructions), MachineState(initialState, 0L, Map())), checkSum)

  }

  val (turingMachine, checksum) = parseInstructions(input)

  val result = process(turingMachine, checksum)

  println(result.machineState.values.filter(_._2).size)


}
