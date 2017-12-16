package Year2017

import scala.io.Source

object Day8 extends App with Benchmark {

  // b inc 5 if a > 1
  // <= < >= > != ==
  val reg =
  """(\w+)\s+(inc|dec)\s+(-?\d+)\s+if\s+(\w+)\s+([<>=!]+)\s+(-?\d+)""".r

  val instructions = Source.fromFile("data/Year2017/Day8.txt").getLines()
    .map {
      case reg(targetRegistry, operation, operationValue, testRegistry, comparison, conditionValue) =>
        IncDecOnCondition(targetRegistry, Operation(operation), operationValue.toInt, testRegistry, Comparison(comparison), conditionValue.toInt)
      case _ => Nop
    }

  val finalRegistries = instructions.foldLeft(Map[String, Int]("max" -> Integer.MIN_VALUE))((s, i) => i.execute(s))

  println(finalRegistries.maxBy(x => if (x._1 == "max") Integer.MIN_VALUE else x._2))
  println(finalRegistries.maxBy(_._2))

  sealed trait Instruction {
    def execute(registers: Map[String, Int]): Map[String, Int]
  }

  case object Nop extends Instruction {
    override def execute(registers: Map[String, Int]): Map[String, Int] = registers
  }

  case class IncDecOnCondition(targetRegistry: String, operation: Operation, operationValue: Int, testRegistry: String, comparison: Comparison, comparisonValue: Int) extends Instruction {
    override def execute(registers: Map[String, Int]): Map[String, Int] = {
      val registryModifier = operation match {
        case Inc => (x: Int) => x + operationValue
        case Dec => (x: Int) => x - operationValue
      }

      val registryCondition = comparison match {
        case Gt => (l: Int, r: Int) => l > r
        case Gte => (l: Int, r: Int) => l >= r
        case Lt => (l: Int, r: Int) => l < r
        case Lte => (l: Int, r: Int) => l <= r
        case Eq => (l: Int, r: Int) => l == r
        case Ne => (l: Int, r: Int) => l != r
      }

      val testValue = registers.getOrElse(testRegistry, 0)

      if (registryCondition(testValue, comparisonValue)) {
        val newValue = registryModifier(registers.getOrElse(targetRegistry, 0))

        (if (newValue > registers("max")) {
          registers + (("max", newValue))
        } else {
          registers
        }) + ((targetRegistry, newValue))
      } else {
        registers
      }
    }
  }

  sealed trait Operation

  object Operation {

    def apply(s: String): Operation = {
      s match {
        case "inc" => Inc
        case "dec" => Dec
      }
    }
  }

  case object Inc extends Operation

  case object Dec extends Operation

  sealed trait Comparison

  object Comparison {

    def apply(s: String): Comparison = {
      s match {
        case ">" => Gt
        case ">=" => Gte
        case "<" => Lt
        case "<=" => Lte
        case "==" => Eq
        case "!=" => Ne
      }
    }
  }

  case object Gt extends Comparison

  case object Gte extends Comparison

  case object Lt extends Comparison

  case object Lte extends Comparison

  case object Ne extends Comparison

  case object Eq extends Comparison

}
