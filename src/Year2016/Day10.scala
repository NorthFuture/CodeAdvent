package Year2016

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App  {

  val regValue ="""value\s+(\d+)\s+goes to bot\s+(\d+)""".r
  val regGiveBot ="""bot\s+(\d+)\s+gives low to bot\s+(\d+)\s+and high to bot\s+(\d+)""".r
  val regGiveOutputLow ="""bot\s+(\d+)\s+gives low to output\s+(\d+)\s+and high to bot\s+(\d+)""".r
  val regGiveOutputBoth ="""bot\s+(\d+)\s+gives low to output\s+(\d+)\s+and high to output\s+(\d+)""".r

  def getBotData(bot: Int, bots: Map[Int, Seq[Int]]): Seq[Int] = {
    bots.getOrElse(bot, Seq[Int]())
  }

  def updateBotData(bot: Int, bots: BotState, updater: Seq[Int] => Seq[Int]): BotState = {
    val currentBotData = bots.getOrElse(bot, Seq[Int]())

    val newBotData = updater(currentBotData)

    bots + ((bot, newBotData))
  }

  def updateOutputData(outputSlot: Int, outputs: OutputState, updater: Seq[Int] => Seq[Int]): OutputState = {
    val currentOutputtData = outputs.getOrElse(outputSlot, Seq[Int]())

    val newBotData = updater(currentOutputtData)

    outputs + ((outputSlot, newBotData))
  }

  val instructions = Source.fromFile("data/Day11.txt").getLines().toList
    .map { x =>
      x match {
        case regValue(value, bot) => Some(Value(value.toInt, bot.toInt))

        case regGiveBot(bot, low, high) => Some(GivesBot(bot.toInt, low.toInt, high.toInt))

        case regGiveOutputLow(bot, low, high) => Some(GivesLow(bot.toInt, low.toInt, high.toInt))

        case regGiveOutputBoth(bot, low, high) => Some(GivesBoth(bot.toInt, low.toInt, high.toInt))
        case _ => None
      }
    }

  type BotState = Map[Int, Seq[Int]]
  type OutputState = Map[Int, Seq[Int]]

  val initInstructions: Seq[Value] = instructions.collect { case i@Some(x: Value) => x
  }

  val otherInstructions: Seq[Instruction] = instructions.flatten.collect { case i: GivesBot => i
  case i: GivesLow => i
  case i: GivesBoth => i
  }

  val initialState = initInstructions
    .foldLeft(Map[Int, Seq[Int]](): BotState, Map[Int, Seq[Int]](): OutputState)((state, i) => {
      (processValue(i, state._1, state._2))
    })


  val finalState = processInstruction(initialState._1, initialState._2, otherInstructions)


  println("Bots ")
  finalState._1.foreach(println)

  println("Outputs ")
  finalState._2.foreach(println)

  println(finalState._2(0).head * finalState._2(1).head * finalState._2(2).head)

  def processValue(instruction: Value, botState: BotState, outputState: OutputState): (BotState, OutputState) = {
    (updateBotData(instruction.bot, botState, z => z :+ instruction.value), outputState)
  }

  def processGivesBot(activeBot: Int, instruction: GivesBot, botState: BotState, outputState: OutputState): (BotState, OutputState) = {

    val low = getBotData(activeBot, botState).min // there's always a min

    val high = getBotData(activeBot, botState).max // there's always a max

    val newBotState_1 = updateBotData(activeBot, botState, z => Seq())

    val newBotState_2 = updateBotData(instruction.low, newBotState_1, z => z :+ low)

    val newBotState_3 = updateBotData(instruction.high, newBotState_2, z => z :+ high)

    (newBotState_3, outputState)
  }

  def processGivesLow(activeBot: Int, instruction: GivesLow, botState: BotState, outputState: OutputState): (BotState, OutputState) = {

    val low = getBotData(activeBot, botState).min // there's always a min

    val high = getBotData(activeBot, botState).max // there's always a max

    val newBotState_1 = updateBotData(activeBot, botState, z => Seq())

    val newOutputState = updateOutputData(instruction.low, outputState, z => z :+ low)

    val newBotState_3 = updateBotData(instruction.high, newBotState_1, z => z :+ high)

    (newBotState_3, newOutputState)
  }

  def processGivesBoth(activeBot: Int, instruction: GivesBoth, botState: BotState, outputState: OutputState): (BotState, OutputState) = {

    val low = getBotData(activeBot, botState).min // there's always a min

    val high = getBotData(activeBot, botState).max // there's always a max

    val newBotState_1 = updateBotData(activeBot, botState, z => Seq())

    val newOutputState_1 = updateOutputData(instruction.low, outputState, z => z :+ low)

    val newOutputState_2 = updateOutputData(instruction.high, outputState, z => z :+ high)

    (newBotState_1, newOutputState_2)
  }

  @tailrec
  def processInstruction(botState: BotState, outputState: OutputState, instructions: Seq[Instruction]): (BotState, OutputState) = {

    val activeBot = botState.filter(x => x._2.length == 2).headOption

    activeBot match {
      case None => (botState, outputState) // it's over
      case Some(x) =>

        if (x._2.contains(61) && x._2.contains(17)) {
          System.out.println(x._1)
        }

        val instruction = instructions.filter(_.bot == x._1).headOption

        val newState = instruction match {
          case Some(x: GivesBot) => processGivesBot(x.bot, x, botState, outputState)
          case Some(x: GivesLow) => processGivesLow(x.bot, x, botState, outputState)
          case Some(x: GivesBoth) => processGivesBoth(x.bot, x, botState, outputState)
        }

        processInstruction(newState._1, newState._2, instructions)
    }
  }

  sealed trait Instruction {
    val bot: Int
  }

  case class Value(value: Int, bot: Int) extends Instruction

  case class GivesBot(bot: Int, low: Int, high: Int) extends Instruction

  case class GivesLow(bot: Int, low: Int, high: Int) extends Instruction

  case class GivesBoth(bot: Int, low: Int, high: Int) extends Instruction

}
