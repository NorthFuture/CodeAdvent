def part2() = {
  def isPrime(n: Int) = (2 to math.sqrt(n).toInt) forall (n % _ != 0)

  var cnt = 0
  for (candidate <- 106700 to (17000+106700) by 17)
    if (!isPrime(candidate)) cnt += 1
  cnt
}