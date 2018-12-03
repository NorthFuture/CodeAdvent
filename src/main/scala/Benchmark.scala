trait Benchmark {

  def benchmark[T](x: => T): T = {

    val start = System.nanoTime()

    val r = x

    val end = System.nanoTime()

    System.out.println((end - start) / 1000 / 1000 + "ms")

    r
  }

}
