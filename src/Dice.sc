val r = new scala.util.Random(System.currentTimeMillis())

val count = 10000000
val n_throws = 11
val n_dice = 9

/*
(1 to count)
  .map { _ => r.nextInt(6) }
  .map {
    _ + 1
  }
  .groupBy(x => x).map(x => (x._1, x._2.size))
  .map { x => (x._1, x._2 * 100.0 / count) }
  .toList.sortBy(_._1)
  .foreach(println(_))
*/

def throw_dice(n: Int) = {
  (1 to n)
    .map { _ => r.nextInt(6) }
    .map {
      _ + 1
    }
    .toList
}

case class SimulationResult(length: Double, shorted: Int, longest: Int, average: Int, opportunity_count: Int)


def simulate_one(): SimulationResult = {

  val throws = (1 to n_throws)
    .map { _ => throw_dice(n_dice) }


  //  val snake_eyes = throws.filter(_.forall(x => x == 1))

  val snake_eyes = throws.filter(_.filter(x => x == 1).size>2)

  val others = throws.filter(_.exists(x => x != 1)).map(x => x.foldLeft(1)((a, x) => a + x))

  SimulationResult(
    others.sum,
    others.min,
    others.max,
    others.sum / others.size,
    snake_eyes.size
  )
}


val rr = (1 to count)
  .map(_ => simulate_one())
  .foldLeft(SimulationResult(0, 0, 0, 0, 0))((a: SimulationResult, x: SimulationResult) =>
    SimulationResult(
      a.length + x.length,
      a.shorted + x.shorted,
      a.longest + x.longest,
      a.average + x.average,
      a.opportunity_count + x.opportunity_count)
  )

println(SimulationResult(rr.length / count / 30.0, rr.shorted / count, rr.longest / count, rr.average / count, rr.opportunity_count / count))

