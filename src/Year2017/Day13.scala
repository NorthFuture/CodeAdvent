package Year2017

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App with Benchmark {

  sealed trait Direction {
  }

  case object Up extends Direction

  case object Down extends Direction

  case class Layer(depth: Int, range: Int, scannerPosition: Int = 0, direction: Direction = Down, detected: Boolean = false) {
    def tick(): Layer = {
      direction match {
        case Down if scannerPosition == range - 1 => this.copy(direction = Up, scannerPosition = scannerPosition - 1)
        case Down => this.copy(scannerPosition = this.scannerPosition + 1)
        case Up if scannerPosition == 0 => this.copy(direction = Down, scannerPosition = 1)
        case Up => this.copy(scannerPosition = this.scannerPosition - 1)
      }
    }
  }

  case class State(layers: Map[Int, Layer], currentLayer: Int) {

    def maxLayer: Int = layers.keys.max

    def tick(): State = {

      val detectedState = this.layers.get(currentLayer) match {
        case Some(x) if x.scannerPosition == 0 =>
          this.copy(layers = this.layers + ((x.depth, x.copy(detected = true))))
        case _ =>
          this
      }

      detectedState.copy(currentLayer = detectedState.currentLayer + 1,
        layers = detectedState.layers.mapValues(_.tick())
      )
    }

    def detectedLayers(): Seq[Layer] = {
      this.layers.filter(_._2.detected).map(_._2).toSeq
    }


    def score(): Int = {
      detectedLayers.map(x => x.range * x.depth).sum
    }
  }

  val layerConfiguration = Source.fromFile("data/Year2017/Day13.txt").getLines()
    .map(_.split(":"))
    .map(x => Layer(x(0).trim.toInt, x(1).trim.toInt))
    .map(x => x.depth -> x)
    .toMap

  @tailrec
  def solvePart1(currentLayer: Int, delay: Int, maxLayer: Int, layers: Map[Int, Layer], detectedLayers: Seq[Layer]): Seq[Layer] = {
    if (currentLayer > maxLayer) {
      detectedLayers
    } else {

      val newDetectedLayers = layers.get(currentLayer) match {
        case Some(x) =>
          if ((currentLayer + delay) % (2 * x.range - 2) == 0) {
            detectedLayers :+ x.copy(detected = true)
          } else {
            detectedLayers
          }
        case None => detectedLayers
      }

      solvePart1(currentLayer + 1, delay, maxLayer, layers, newDetectedLayers)
    }
  }

  val resultPart1 = solvePart1(0, 0, layerConfiguration.keys.max, layerConfiguration, Seq())

  println(resultPart1.map(x => x.depth * x.range).sum)


  @tailrec
  def solvePart2(maxLayer: Int, layers: Map[Int, Layer], delay: Int): Int = {

    val detectedLayers = solvePart1(0, delay, layerConfiguration.keys.max, layerConfiguration, Seq())

    detectedLayers.isEmpty match {
      case true =>
        delay
      case _ =>
        solvePart2(maxLayer, layers, delay + 1)
    }
  }

  val resultPart2 = solvePart2(layerConfiguration.keys.max, layerConfiguration, 0)

  println(resultPart2)

}
