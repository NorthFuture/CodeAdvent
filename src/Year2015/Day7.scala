
package codeadvent

import scala.io.Source
import java.util.regex.Matcher
import java.util.regex.Pattern
import scala.collection.mutable.HashMap
import scala.util.Try

object Day7 {

  def input = Source.fromFile("day7.txt").getLines()

  val regSet = """(\d*|\w*)\s*->\s*(.\w)""".r
  val regBinaryOperator = """(.*?)\s*(AND|OR|LSHIFT|RSHIFT)\s*(.*?)\s*->\s*(.*?)""".r
  val regUnaryOperator = """(NOT)\s*(.*?)\s*->\s*(.*?)""".r

  case class Instruction(deps: Seq[String], text: String, func: () => Unit)

  def Step1() {

    val variables = new HashMap[String, Integer]()

    val instructions = input.map { x =>

      def getValue(key: String) = {

        if (isInt(key)) {
          key.toInt
        } else {
          val v = variables.get(key)

          if (v.isEmpty) {
            throw new RuntimeException(x)
          } else {
            v.get.toInt
          }
        }
      }

      def isInt(v: String) = {
        try {
          v.trim().toInt
          true
        } catch {
          case e: Exception =>
            false
        }
      }

      def buildInstruction(var1: String, var2: String, dest: String, f: (Int, Int) => Int) = {

        val deps = (if (isInt(var1)) Seq() else Seq(var1)) ++ ((if (isInt(var2)) Seq() else Seq(var2)))

        Instruction(deps, x, () => {
          variables.put(dest, f(getValue(var1), getValue(var2)))
        })
      }

      x match {
        case regSet(source, dest) =>

          if (isInt(source)) {
            Instruction(Seq(), x, () => { variables.put(dest.trim(), source.trim().toInt) })
          } else {
            Instruction(Seq(source), x, () => { variables.put(dest.trim(), getValue(source)) })
          }

        case regBinaryOperator(var1, op, var2, dest) =>

          op match {
            case "AND" => buildInstruction(var1, var2, dest, (a, b) => a & b)

            case "OR" => buildInstruction(var1, var2, dest, (a, b) => a | b)

            case "LSHIFT" => buildInstruction(var1, var2, dest, (a, b) => a << b)

            case "RSHIFT" => buildInstruction(var1, var2, dest, (a, b) => (a >> b) & 0xFFFF)

          }

        case regUnaryOperator(op, var1, dest) =>

          op match {
            case "NOT" =>

              Instruction(Seq(var1), x, () => {
                variables.put(dest.trim(), ~getValue(var1) & 0xFFFF)
              })

            case _ => Instruction(Seq(), x, () => {})

          }
      }
    }.toList

    def run(instructions: Seq[Instruction]) {

      if (instructions.isEmpty) {
        Seq()
      } else {

        val leftInstructions = instructions.filter { x =>

          val xx = x;

          if (x.deps.size > 0 && x.deps.count { x => variables.get(x).isEmpty } > 0) {
            true
          } else {
            x.func()

            println(x.text)

            false
          }
        }
        run(leftInstructions)
      }
    }

    run(instructions)

    val r = variables.get("a")

    println(s"$r")
  }

  def Step2() {

  }

  def main(args: Array[String]): Unit = {
    Step1()
  }
}