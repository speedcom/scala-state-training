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