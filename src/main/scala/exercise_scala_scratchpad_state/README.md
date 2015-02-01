# Exercise: functional state in Scala

Given a stateful function `run` and a class `State` that wraps it:

```scala
case class State[A,S](run: S => (A,S)) {

  def map[B](g: A => B): State[B,S] = ???

  def flatMap[B](g: A => State[B,S]): State[B,S] = ???

}
```

Implement `map` and `flatMap` so that we can use `State` in a `for`-comprehension:

```scala
def put(k: String, v: Int): State[Int, Map[String,Int]] =
  State(m => (v, m + (k -> v)))

def get(k: String): State[Int, Map[String,Int]] =
  State(m => (m(k), m))

def getAndDouble(k: String): State[Int, Map[String,Int]] =
  State({ m =>
    val v = m(k)
    (v, m + (k -> v * 2))
  })

val resultS: State[Tuple5[Int,Int,Int,Int,Int], Map[String,Int]] =
  for {
    a <- put("foo", 21)      // a = 21, state = Map(foo -> 21)
    b <- get("foo")          // b = 21, state = Map(foo -> 21)
    c <- getAndDouble("foo") // c = 21, state = Map(foo -> 42)
    d <- getAndDouble("foo") // d = 42, state = Map(foo -> 84)
    e <- get("foo")          // e = 84, state = Map(foo -> 84)
  } yield (a,b,c,d,e)

println(resultS.run(Map.empty)) // ((21,21,21,42,84),Map(foo -> 84))
```