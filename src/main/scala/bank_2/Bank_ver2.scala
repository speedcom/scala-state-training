package bank_2

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, s1) = run(s)
    (f(a), s1)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, s1) = run(s)
    f(a).run(s1)
  }
}

object AccountOperation {

  type Account = String
  type TransactionHistory = List[Float]
  type Tx[A] = State[TransactionHistory, A]

  def contribute(x: Float): Tx[Unit] = State { account =>
    ((), account :+ x)
  }

  def balance: Tx[Float] = State { account =>
    (account.sum, account)
  }

  def deduct(x: Float): Tx[Float] = State { account =>
    if(account.sum >= x)
      (x, account :+ (-x))
    else
      (0, account)
  }

  def deposit(x: Float): Tx[(Float, Float)] =
    for {
      _ <- contribute(x)
      b <- balance
    } yield (0f, b)

  def withdraw(x: Float): Tx[(Float, Float)] =
    for {
      y <- deduct(x)
      b <- balance
    } yield (y, b)

  def depositThenWithdraw(d: Float, w: Float): Tx[(Float, Float)] =
    for {
      _  <- deposit(d)
      ww <- withdraw(w)
    } yield ww

}

object BankOperation {
  import AccountOperation._
  
  type Bank = Map[Account, TransactionHistory]
  type Bx[A] = State[Bank, A]

}

object BankRoot {
  import BankOperation._

  val bank: Bank = Map(
    "Matt"   -> List(50f),
    "Justin" -> List(50f))

}

object BankApp extends App {
  import BankRoot._
  import AccountOperation._

  // TODO need to persist changes
  object UcContributeCash {
    def execute(bankAccount: Account, d: Float) = {
      var account = bank(bankAccount)
      val (_, acc) = contribute(d).run(account)
      account = acc
    }
  }

  println(s"Bank: $bank")
  UcContributeCash.execute("Matt", 100f)
  println(s"Bank: $bank")

}

