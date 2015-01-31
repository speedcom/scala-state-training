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