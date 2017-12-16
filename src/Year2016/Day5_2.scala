package Year2016

import scala.annotation.tailrec

object Day5_2 extends App {

  def MD5(v: String) = java.security.MessageDigest.getInstance("MD5").digest(v.getBytes).view.map(0xFF & _).map {
    "%02x".format(_)
  }.foldLeft("") {
    _ + _
  }

  @tailrec
  def crackBlock(doorID: String, i: Int, max: Int, currentPassword: Seq[(Char, Int)]): (Seq[(Char, Int)], Int) = {
    val hash = MD5(doorID + i)
    val newPassword =
      if (hash.startsWith("00000")) {

        System.out.println(hash)

        val charPos = (hash.charAt(5).toInt - '0'.toInt)

        if (charPos < 0 || charPos > 7) {
          currentPassword
        } else {
          currentPassword :+ (hash.charAt(6), charPos)
        }

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
    def internalCrack(doorID: String, step: Int, status: (Seq[(Char, Int)], Int)): (Seq[(Char, Int)], Int) = {
      val blockSize = 50000

      val stepSize = 5

      val result = (step to (step + stepSize)).par.map { x =>
        crackBlock(doorID, x * blockSize, (x + 1) * blockSize - 1, Seq())
      }.seq.foldLeft(status)((x, y) => (x._1 ++ y._1.filter(z => !x._1.map(_._2).contains(z._2)), x._2 + y._2))

      if (result._1.length >= passwordLength) {
        result
      } else {
        internalCrack(doorID, step + stepSize + 1, result)
      }
    }

    val result = internalCrack(doorID, 0, (Seq(), 0))

    System.out.println(result)
    val password = result._1.take(passwordLength).sortBy(_._2).map(_._1).mkString("")

    (password, result._2)
  }

  val start = System.nanoTime()

  val result = crack("cxdnnyjw")

  val end = System.nanoTime()

  val elapseds = (end - start) / 1000.0 / 1000
  System.out.println("CRACKED " + result._1 + " in " + elapseds + " s " + result._2 * 1.0 / elapseds + " op/s")

}
