package Year2016

import scala.annotation.tailrec

object Day5 extends App {

  def MD5(v: String) = java.security.MessageDigest.getInstance("MD5").digest(v.getBytes).view.map(0xFF & _).map {
    "%02x".format(_)
  }.foldLeft("") {
    _ + _
  }

  @tailrec
  def crackBlock(doorID: String, i: Int, max: Int, currentPassword: String): (String, Int) = {
    val hash = MD5(doorID + i)
    val newPassword =
      if (hash.startsWith("00000")) {
        currentPassword + hash.charAt(5)
      } else {
        currentPassword
      }

    //   if (newPassword.length < passwordLength) {
    if (i < max) {
      crackBlock(doorID, i + 1, max, newPassword)
    } else {
      (newPassword, i)
    }
  }


  def crack(doorID: String): (String, Int) = {
    val passwordLength = 8

    @tailrec
    def internalCrack(doorID: String, step: Int, status: (String, Int)): (String, Int) = {
      val blockSize = 50000

      val stepSize = 5

      val blockRresults = (step to (step + stepSize)).par.map { x =>
        crackBlock(doorID, x * blockSize, (x + 1) * blockSize - 1, "")
      }.seq.foldLeft(("", 0))((x, y) => (x._1 + y._1, x._2 + y._2))

      val result = (status._1 + blockRresults._1, status._2 + blockRresults._2)

      if (result._1.length >= passwordLength) {
        (result._1.take(passwordLength), result._2)
      } else {
        internalCrack(doorID, step + stepSize + 1, result)
      }
    }

    internalCrack(doorID, 0, ("", 0))
  }

  val start = System.nanoTime()

  val result = crack("cxdnnyjw")

  val end = System.nanoTime()

  val elapseds = (end - start) / 1000.0 / 1000
  System.out.println("CRACKED " + result._1 + " in " + elapseds + " s " + result._2 * 1.0 / elapseds + " op/s")

}
