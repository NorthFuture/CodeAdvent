package Year2017

import scala.annotation.tailrec
import scala.collection.immutable.Stream.Empty
import scala.collection.mutable.Map
import scala.io.Source

object Day7 extends App with Benchmark {

  val reg ="""(\w+)\s\((\d+)\)\s*[->]*(.*)""".r

  case class Node[T](value: T, children: Seq[Node[T]])

  case class Disc(name: String, weight: Int, childrenWeight: Int, uppernames: Seq[String])

  val nodes = Source.fromFile("data/Year2017/Day7Test.txt").getLines()
    .flatMap { x =>
      x match {
        case reg(name, weight, upperNames) => Some(Disc(name, weight.toInt, 0, upperNames.split(",").filter(!_.isEmpty).map(x => x.trim).toList))
        case _ => System.out.println("malformed " + x); Option.empty[Disc]
      }
    }.toStream

  def mapNodesToTree(nodes: Stream[Disc]): Seq[Node[Disc]] = {

    @tailrec
    def connectDiscs(nodes: Stream[Disc], map: Map[Disc, Seq[Disc]], attachedNodes: Seq[Disc]): (Map[Disc, Seq[Disc]], Seq[Disc]) = {
      nodes match {
        case Empty => (map, attachedNodes)
        case h #:: tail =>
          val childrenDisks = h.uppernames
            .flatMap(x => map.find(_._1.name == x).map(_._1))

          connectDiscs(tail, map + ((h, childrenDisks)), attachedNodes ++ childrenDisks)
      }
    }

    def transformMapToTree(node: Disc, map: Map[Disc, Seq[Disc]]): Node[Disc] = {
      Node(value = node, children = map(node).map(transformMapToTree(_, map)))
    }

    val (connectedMap, attachedNodes) = connectDiscs(nodes, collection.mutable.Map[Disc, Seq[Disc]](nodes.map(x => (x, Seq[Disc]())): _*), Seq())

    connectedMap.filter(x => !attachedNodes.contains(x._1)).map(_._1).map(x => transformMapToTree(x, connectedMap)).toList
  }

  def mapWeightToChildrenWeight(node: Node[Disc]): Node[Disc] = {

    val childrenSums = node.children.map(mapWeightToChildrenWeight)

    val childrenResult = childrenSums
      .foldLeft(("", 0))((s, x) => (s._1 + x.value.name, s._2 + x.value.childrenWeight))

    node.copy(children = childrenSums, value = node.value.copy(childrenWeight = childrenResult._2 + node.value.weight))
  }

  def findUnbalance(node: Node[Disc]): Option[(Node[Disc], Int)] = {

    val groupBy = node.children.groupBy(_.value.childrenWeight).toList.sortBy(_._2.size)

    if (groupBy.size > 1) {
      val nodeToFix = groupBy.head._2.head
      val okNode = groupBy.tail.head._2.head

      findUnbalance(nodeToFix) match {
        case None =>
          Some((nodeToFix, nodeToFix.value.childrenWeight - okNode.value.childrenWeight))
        case Some(x) =>
          Some(x)
      }
    } else {
      None
    }
  }

  val trees = mapNodesToTree(nodes)

  val childrenWeightTrees = trees.map(mapWeightToChildrenWeight)

  val unbalances = childrenWeightTrees.map(findUnbalance)

  unbalances.map(x => x.map(x => x._1.value.name + "-> from " + x._1.value.weight + " to " + (x._1.value.weight - x._2)))
    .map(_.getOrElse("No unbalance found")).foreach(println)

}
