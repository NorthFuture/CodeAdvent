
package codeadvent

import java.security.MessageDigest
object Day4 {

  val input = "bgvyzdsv"

  val md5 = MessageDigest.getInstance("MD5")

  def Step1() {

    for (i <- 0 to Int.MaxValue - 1) {
      val s = input + i

      val h = MessageDigest.getInstance("MD5").digest(s.getBytes).map(0xFF & _).map { "%02x".format(_) }.foldLeft("") { _ + _ }

      if (h.startsWith("00000")) {
        throw new RuntimeException("" + i)
      }

    }

  }

  def Step2() {
    for (i <- 0 to Int.MaxValue - 1) {
      val s = input + i

      val h = MessageDigest.getInstance("MD5").digest(s.getBytes).map(0xFF & _).map { "%02x".format(_) }.foldLeft("") { _ + _ }

      if (h.startsWith("000000")) {
        throw new RuntimeException("" + i)
      }

    }
  }

  def main(args: Array[String]): Unit = {
  //  Step1()
    Step2()
  }
}