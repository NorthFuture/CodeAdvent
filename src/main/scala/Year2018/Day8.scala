package Year2018

import scala.annotation.tailrec
import scala.io.Source

object Day8 extends App {

  val input = Source.fromFile("data/Year2018/Day8.txt").getLines().map(_.split(" ").map(_.toInt)).map(solve2(_)).foreach(println)

  case class NodeHeader(childCount: Int, metaCount: Int) {
    def dec(): NodeHeader = {
      copy(childCount = childCount - 1)
    }
  }

  case class Node(header: NodeHeader, childrenSum: Seq[Int] = Seq()) {
    def dec(): Node = {

      copy(header = header.dec())
    }

    def addSum(s: Int): Node = {
      copy(childrenSum = childrenSum :+ s)
    }
  }

  def solve1(s: Seq[Int]): Int = {

    // 2 3 0 3 10 11 12 1 1 0  1 99  2  1  1  2
    // 0 1 2 3  4  5  6 7 8 9 10 11 12 13 14 15

    @tailrec
    def internal(index: Int, nodeDone: Boolean, nodes: Seq[Node], sum: Seq[Int]): Int = {

      if (index == s.length) {
        sum.sum
      } else if (nodeDone) {
        val newNode = nodes.head.dec()

        internal(index, false, newNode +: nodes.tail, sum)
      } else if (nodes.headOption.exists(_.header.childCount == 0)) {
        val parentNodeSum = s.slice(index, index + nodes.head.header.metaCount).sum
        internal(index + nodes.head.header.metaCount, true, nodes.tail, sum :+ parentNodeSum)
      } else {
        val nodeHeader = NodeHeader(s(index), s(index + 1))
        val node = Node(header = nodeHeader)

        if (nodeHeader.childCount == 0) {
          val nodeSum = s.slice(index + 2, index + 2 + nodeHeader.metaCount).sum

          internal(index + 2 + nodeHeader.metaCount, true, nodes, sum :+ nodeSum)
        } else {
          internal(index + 2, false, node +: nodes, sum)
        }
      }
    }

    internal(0, false, Seq(), Seq())
  }

  def solve2(s: Seq[Int]): Int = {

    // 2 3 0 3 10 11 12 1 1 0  1 99  2  1  1  2
    // 0 1 2 3  4  5  6 7 8 9 10 11 12 13 14 15

    @tailrec
    def internal(index: Int, nodeDone: Boolean, nodes: Seq[Node]): Int = {
      if (nodeDone) {
        val newNode = nodes.head.dec()

        internal(index, false, newNode +: nodes.tail)
      } else if (nodes.headOption.exists(_.header.childCount == 0)) {
        val parentNodeSum = s.slice(index, index + nodes.head.header.metaCount).flatMap(x => nodes.head.childrenSum.lift(x - 1)).sum

        if (nodes.tail.isEmpty) {
          parentNodeSum
        } else {
          internal(index + nodes.head.header.metaCount, true, nodes.tail.head.addSum(parentNodeSum) +: nodes.tail.tail)
        }
      } else {
        val nodeHeader = NodeHeader(s(index), s(index + 1))
        val node = Node(header = nodeHeader)

        if (nodeHeader.childCount == 0) {
          val nodeSum = s.slice(index + 2, index + 2 + nodeHeader.metaCount).sum

          internal(index + 2 + nodeHeader.metaCount, true, nodes.head.addSum(nodeSum) +: nodes.tail)
        } else {
          internal(index + 2, false, node +: nodes)
        }
      }
    }

    internal(0, false, Seq())
  }

}