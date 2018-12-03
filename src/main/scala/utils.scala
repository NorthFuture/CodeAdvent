import scala.annotation.tailrec

object T {


  def shiftList(list: List[Int], count: Int): List[Int] = {

    @tailrec
    def internal(list: List[Int], totalLength: Int, currentPosition: Int, tailResult: List[Int], headResult: List[Int]): List[Int] = {

      list match {
        case Nil =>
          headResult ::: tailResult
        case h :: tail if currentPosition >= totalLength =>
          internal(tail, totalLength, currentPosition + 1, tailResult, headResult :+ h)
        case h :: tail =>
          internal(tail, totalLength, currentPosition + 1, tailResult :+ h, headResult)
      }
    }

    internal(list, list.length, count, List(), List())
  }
}