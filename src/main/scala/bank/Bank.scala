package bank

/**
 * DRAWBACKS
 * Mutable: account reference can change any time
 * Imperative: evaluating deposit makes the transaction happen
 * Inconsistent: account reference is accessed multiple times
 */
object Bank_1 {

  type Account = List[Float]

  var account: Account = List()

  def deposit(x: Float): Float = {
    account = account :+ x
    account.sum
  }

}


/**
 * IMPROVEMENTS
 * Immutable: account reference can not change
 * Declarative: evaluating deposit does not run the transaction
 * Consistent: account accesses are certain to be identical
 */
object Bank_2 {

  type Account = List[Float]

  def deposit(x: Float): Account => (Float, Account) = {
    account => (account.sum, account :+ x)
  }
}

/**
 * Representing a state action
 * A State wraps a function run which, given an S, performs
 * some computation and produces an A and s new S.
 */
case class State[S,A](run: S => (A,S)) {

  /*
  map builds a new state action that, once run, has its output
  run through f, converting it from A to B
   */
  def map[B](f: A => B): State[S, B] =
    State { run andThen { case (a, s) => (f(a), s) }}


}

// Composing with pure functions
object StateMapExample extends App {

  type Account = List[Float]
  type Tx[A] = State[Account, A]

  val balance: Tx[Float] = State { account =>
    (account.sum, account)
  }

  val report: Tx[String] = balance map { sum => "Your balance is " + sum }

}









