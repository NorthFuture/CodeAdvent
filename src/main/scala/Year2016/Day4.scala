package Year2016

import scala.io.Source

object Day4 extends App {

  val reg ="""([a-z\-]+)\-(\d+)\[(.*?)\]""".r

  val regName ="""([a-z]+)\-?""".r

  case class Room(roomName: String, roomNumber: Int, checkSum: String)

  val realRooms = Source.fromFile("data/Day4.txt").getLines().map { x =>
    x match {
      case reg(x1, x2, x3) => Some(Room(x1, x2.toInt, x3))
      case _ => System.out.println(s"Malformed ${x}"); None
    }
  }.flatten.map { x =>
    (x, x.roomName.split("-").flatMap(x => x.toList).groupBy(identity).map(y => (y._1, y._2.size)).toList.sortBy(w => (-1 * w._2, w._1)).take(5).map(_._1)
    )
  }.filter { x =>
    x._1.checkSum.forall(y => x._2.contains(y))
  }.map {
    _._1
  }.toList

  var result1 = realRooms.map {
    _.roomNumber
  }.sum

  System.out.println(result1)

  var result2 = realRooms.map { x =>
    x.roomNumber + " - " +
      x.roomName.map { c =>
        c match {
          case '-' => ' '
          case y: Char => (y + (x.roomNumber % 26)).toChar
        }
      }
  }

  System.out.println(result2.mkString("\n"))

}
