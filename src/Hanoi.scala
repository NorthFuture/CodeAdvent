

object Hanoy extends App with Benchmark {

  type State = Seq[Slot]

  case class Slot(slotId: Int, disks: List[Int]) {
    def pop(): (Int, Slot) = {
      (disks.head, this.copy(disks = disks.tail))
    }

    def push(v: Int): Slot = {
      this.copy(disks = v :: this.disks)
    }
  }

  def printTowers(state: State) = {

    val maxSize = 3

    def printDisc(size: Option[Int]) = {

      val disc = size.map("*" * _).getOrElse(" " * maxSize)

      val paddingLength = (maxSize + 4 - disc.length) / 2

      print(" " * paddingLength + disc + " " * paddingLength)

    }

    state.map(s => List.fill(maxSize - s.disks.length)(Option.empty[Int]) ++ s.disks.map(Some(_))).transpose.foreach(r => {
      r.foreach(x => printDisc(x))

      println(" ")
    })

  }

  val initialState: State = Seq(
    Slot(0, List(1, 3, 5)),
    Slot(1, List()),
    Slot(2, List())
  )


  var steps = Seq[(Int, Int, Int)]()

  def move(state: State, n: Int, from: Int, to: Int, aux: Int): State = {

    if (n == 0) {
      state
    } else {
      val state_1 = move(state, n - 1, from, aux, to)

      val (v, newFrom) = state_1(from).pop()

      val newTo = state_1(to).push(v)

      val state_2 = state_1.map(x => x match {
        case x if x == state_1(from) => newFrom
        case x if x == state_1(to) => newTo
        case y: Slot => y
      })

      val state_3 = move(state_2, n - 1, aux, to, from)

      printTowers(state_3)
      state_3
    }
  }

  printTowers(initialState)

  val newState = move(initialState, 3, 0, 2, 1)
}
