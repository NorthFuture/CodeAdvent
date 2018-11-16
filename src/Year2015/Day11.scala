
package Year2015

import scala.annotation.tailrec

object Day11 extends App {


  def isValid(s: String): Boolean = {

    val c1 = !s.contains("i") && !s.contains("l") && !s.contains("o")
    val c2 = s.sliding(3).exists(x => x(0) == (x(1) - 1) && x(1) == (x(2) - 1))
    val c3 = s.sliding(2).filter(x => x.head == x.last).map(_.head).toList.distinct.size > 1

    c1 && c2 && c3
  }

  def _newPassword(s: String): String = {

    val new_char = (s.head + 1)

    if (new_char > 'z') {
      'a' + _newPassword(s.tail)
    } else {
      new_char.toChar + s.tail
    }

  }

  @tailrec
  def newPassword(str: String): String = {

    val newPass = _newPassword(str.reverse).reverse

    if (isValid(newPass)) {
      newPass
    } else {
      newPassword(newPass)
    }

  }

  val seed = "vzbxxyzz"

  println(newPassword(seed))
}