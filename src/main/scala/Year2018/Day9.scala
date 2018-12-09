package Year2018

import scala.annotation.tailrec
import scala.collection.mutable

object Day9 extends App {

  val numPlayers = 405
  val lastMarble = 7170000

  class Node(var prior: Node, var next: Node, val value: Int) {
    def insertAfter(value: Int): Node = {

      next = new Node(this, next, value)

      next.next.prior = next

      next
    }

    def removeNext(): Node = {

      val result = this.next

      next = result.next
      next.prior = this

      result
    }

    def removePrior(): Node = {

      val result = this.prior

      prior = result.prior
      prior.next = this

      result
    }

    override def toString: String = value.toString
  }

  object Node {

    def createRoot(value: Int): Node = {
      val n = new Node(null, null, value)

      n.prior = n
      n.next = n

      n
    }
  }

  class State(var nextFreeMarble: Int, var currentMarblePosition: Node, var currentPlayer: Int, var players: mutable.Map[Int, Long]) {

    def toString(fromNode: Node): String = {

      @tailrec
      def internal(node: Node, acc: String, looped: Boolean): String = {

        def nodeToString(node: Node): String = {
          if (node == currentMarblePosition) s"(${node.value})" else node.value.toString + "  "
        }

        if (node == fromNode && looped) {
          acc
        } else if (node == fromNode && !looped) {
          internal(node.next, acc + nodeToString(node), true)
        } else {
          internal(node.next, acc + nodeToString(node), looped)
        }
      }

      internal(fromNode, "", false)
    }

    @inline
    def placeMarble(): State = {

      currentMarblePosition = currentMarblePosition.next.insertAfter(nextFreeMarble)

      nextFreeMarble = nextFreeMarble + 1

      this
    }

    @inline
    def playSpecialTurn(): State = {

      val node = currentMarblePosition.prior.prior.prior.prior.prior.prior

      val playerScore = node.removePrior().value + nextFreeMarble

      val newPlayers = players.update(currentPlayer, players.getOrElse(currentPlayer, 0L) + playerScore)

      nextFreeMarble = nextFreeMarble + 1

      currentMarblePosition = node

      this
    }

    @inline
    def playTurn(): State = {
      if (nextFreeMarble != 0 && nextFreeMarble % 23 == 0) {
        playSpecialTurn()
      } else {
        placeMarble()
      }
      currentPlayer = (currentPlayer + 1) % numPlayers
      this
    }
  }

  @tailrec
  def play(state: State): State = {
    // println(state.getString)

    if (state.nextFreeMarble > lastMarble) {
      state
    } else {
      play(state.playTurn())
    }
  }

  val initialState = new State(1, Node.createRoot(0), 1, mutable.Map())

  val result = play(initialState)

  println(result.players.maxBy(_._2).toString)
}

