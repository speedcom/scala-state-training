package exercise_scala_scratchpad_state

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, s2) = run(s)
    (f(a), s2)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s1) = run(s)
    f(a).run(s1)
  }

}

object Solution extends App {

  def put(k: String, v: Int): State[Map[String, Int], Int] =
    State(m => (v, m + (k -> v)))

  def get(k: String): State[Map[String, Int], Int] =
    State(m => (m(k), m))

  def getAndDouble(k: String): State[Map[String, Int], Int] =
    State(m => {
      val kk = m(k)
      (kk, m + (k -> kk * 2))
    })

  val resultS: State[Map[String,Int], Tuple5[Int,Int,Int,Int,Int]] =
    for {
      a <- put("foo", 21)            // a = 21, state = Map(foo -> 21)
      b <- get("foo") // error prone // b = 21, state = Map(foo -> 21)
      c <- getAndDouble("foo")       // c = 21, state = Map(foo -> 42)
      d <- getAndDouble("foo")       // d = 42, state = Map(foo -> 84)
      e <- get("foo")                // e = 84, state = Map(foo -> 84)
    } yield (a, b, c, d, e)

  println(resultS.run(Map.empty)) // ((21, 21, 21, 42, 84), Map(foo -> 84))


}