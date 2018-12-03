
package Year2015

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day15 extends App {

  val reg = """(\w*): capacity (-?\d*), durability (-?\d*), flavor (-?\d*), texture (-?\d*), calories (-?\d*)""".r

  case class Ingridient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int) {

    def *(x: Int) = this.copy(name, capacity * x, durability * x, flavor * x, texture * x)
  }

  val ingridients = Source.fromFile("data/Year2015/day15.txt").getLines().map {
    case reg(name, capacity, durability, flavor, texture, calories) => Ingridient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
  }.toList

  def weight_calories(i: Seq[Ingridient], spoons: Seq[Int]): (Int, Int) = {

    val components = (0 to i.size - 1)
      .map(x => (i(x).capacity * spoons(x), i(x).durability * spoons(x), i(x).flavor * spoons(x), i(x).texture * spoons(x), i(x).calories * spoons(x)))
      .foldLeft((0, 0, 0, 0, 0))((s, x) => (s._1 + x._1, s._2 + x._2, s._3 + x._3, s._4 + x._4, s._5 + x._5))

    if (components._1 < 0 || components._2 < 0 || components._3 < 0 || components._4 < 0)
      (0, 0)
    else
      (components._1 * components._2 * components._3 * components._4, components._5)
  }


  @tailrec
  def getNewSpoons(spoons: mutable.Seq[Int], index: Int): Boolean = {

    if (index < spoons.size) {

      spoons(index) = spoons(index) + 1

      if (spoons(index) > 100) {
        spoons(index) = 0
        getNewSpoons(spoons, index + 1)
      } else {
        true
      }
    } else {
      false
    }
  }

  @tailrec
  def solve(i: Seq[Ingridient], spoons: mutable.Seq[Int], bestWeight: ((Int, Int), List[Int])): ((Int, Int), List[Int]) = {
    if (getNewSpoons(spoons, 0)) {

      if (spoons.sum == 100) {

        val new_weight_calories = weight_calories(i, spoons)

        if (new_weight_calories._1 > bestWeight._1._1 && new_weight_calories._2 == 500) {

          solve(i, spoons, (new_weight_calories, spoons.toList))
        } else {
          solve(i, spoons, bestWeight)
        }

      } else {
        solve(i, spoons, bestWeight)
      }
    } else {
      bestWeight
    }

  }


  val res = solve(ingridients, mutable.Seq.fill(ingridients.size)(0), ((0, 0), List()))

  println(res)

}