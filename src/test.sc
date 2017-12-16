


val s = Seq(1, 2, 3, 4, 5)

val x = Seq(1, 2, 10, 11)

(s ++ x) collect {
  case i if !s.contains(i) => i
}
