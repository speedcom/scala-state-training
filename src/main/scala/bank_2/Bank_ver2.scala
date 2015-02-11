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

package object Domain {
  type Account = String
  type TransactionHistory = List[Float]
  type Bank = Map[Account, TransactionHistory]
}

package object StateDomain {
  import Domain._
  type Tx[A] = State[TransactionHistory, A]
  type Bx[A] = State[Bank, A]
}

object AccountOperation {
  import StateDomain.Tx
  
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
  import Domain._
  import StateDomain._

  def findTh(account: Account): Bx[Option[TransactionHistory]] = State { bank =>
    (bank.get(account), bank)
  }

}

object BankRoot {
  import Domain._

  var bank: Bank = Map(
    "Matt"   -> List(50f),
    "Justin" -> List(50f, 100f, -40f, 700f))

  def update(account: Account, th: TransactionHistory): Bank = {
    bank = (bank-account) + (account -> th)
    bank
  }
}

object BankApp extends App {
  import BankRoot._
  import Domain._

  object UcContributeCash {

    def execute(bankAccount: Account, cash: Float) = {

      val (thOption, _) = BankOperation.findTh(bankAccount).run(bank)

      val contributed = thOption.map { th =>
        AccountOperation.contribute(cash).run(th)._2
      }

      contributed.map { th =>
        BankRoot.update(bankAccount, th)
      }

      // TODO want to be working solution
//      for {
//        (thOption, _) <- BankOperation.findTh(bankAccount).run(bank)
//        thOld         <- thOption
//        (_, contr)    <- AccountOperation.contribute(cash).run(thOld)
//        thNew         <- contr
//      } yield BankRoot.update(bankAccount, thNew)


    }
  }

  println(s"Bank: $bank")
  UcContributeCash.execute("Matt", 100f)
  println(s"Bank: $bank")

}

