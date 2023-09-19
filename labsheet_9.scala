import scala.io.StdIn
import scala.io.StdIn._

object labsheet_9 {


  //Q1
  class Rational1(n: Int, d: Int) {
    require(d != 0, "Denominator must be non-zero")

    val numer1: Int = n
    val denom1: Int = d

    def neg: Rational1 = new Rational1(-numer1, denom1)

    override def toString: String = s"$numer1/$denom1"
  }

  //Q2
  class Rational2(num: Int, den: Int) {
    require(den != 0, "Denominator cannot be zero")

    private val gcdValue: Int = gcd(num.abs, den.abs)
    val numer2: Int = num / gcdValue
    val denom2: Int = den / gcdValue

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    def -(that: Rational2): Rational2 = {
      new Rational2(numer2 * that.denom2 - that.numer2 * denom2, denom2 * that.denom2)
    }

    override def toString: String = s"$numer2/$denom2"
  }

  //Q3
  class Account(val accountNumber: String, var balance: Double) {

    def deposit(amount: Double): Unit = {
      if (amount > 0) {
        balance += amount
        println(s"Deposited $amount. New balance: $balance")
      } else {
        println("Invalid deposit amount.")
      }
    }

    def withdraw(amount: Double): Unit = {
      if (amount > 0 && amount <= balance) {
        balance -= amount
        println(s"Withdrew $amount. New balance: $balance")
      } else {
        println("Invalid withdrawal amount.")
      }
    }

    def transfer(amount: Double, toAccount: Account): Unit = {
      if (amount > 0 && amount <= balance) {
        balance -= amount
        toAccount.balance += amount
        println(s"Transferred $amount to ${toAccount.accountNumber}. Your new balance: $balance")
        println(s"Received $amount from ${accountNumber}. New balance: ${toAccount.balance}")
      } else {
        println("Invalid transfer amount.")
      }
    }

    override def toString: String = s"Account($accountNumber) - Balance: $balance"
  }

  //Q4
  case class Account2(accountNumber: Int, balance: Double)

  case class Bank(accounts: List[Account2])

  def getNegativeBalances(bank: Bank): List[Account2] = {
    bank.accounts.filter(_.balance < 0)
  }

  def calculateTotalBalance(bank: Bank): Double = {
    bank.accounts.map(_.balance).sum
  }

  def calculateFinalBalances(bank: Bank): List[Account2] = {
    bank.accounts.map { account =>
      val interestRate = if (account.balance >= 0) 0.05 else 0.1
      val interestAmount = account.balance * interestRate
      Account2(account.accountNumber, account.balance + interestAmount)
    }
  }

  def main(args: Array[String]): Unit = {

    //Q1
    println("Enter numerator:")
    val numerator = StdIn.readInt()

    println("Enter denominator:")
    val denominator = StdIn.readInt()

    val rationalNumber = new Rational1(numerator, denominator)
    val negNumber = rationalNumber.neg

    println(s"The negation of $rationalNumber is $negNumber")

    //Q2
    val x = new Rational2(3, 4)
    val y = new Rational2(5, 8)
    val z = new Rational2(2, 7)

    val result = x - y - z
    println(s"x = $x")
    println(s"y = $y")
    println(s"z = $z")
    println(s"$x - $y - $z = $result")

    //Q3
    val account1 = new Account("123456", 1000.0)
    val account2 = new Account("789012", 500.0)

    println(account1)
    println(account2)
    println()

    account1.deposit(200.0)
    account2.withdraw(100.0)
    println()

    println(account1)
    println(account2)
    println()

    account1.transfer(300.0, account2)
    println()

    println(account1)
    println(account2)


    //Q4
    val accounts = List(
      Account2(1, 1000),
      Account2(2, -500),
      Account2(3, 200),
      Account2(4, -100),
      Account2(5, 1500)
    )

    val bank = Bank(accounts)

    val negativeBalances = getNegativeBalances(bank)
    println("Accounts with negative balances:")
    negativeBalances.foreach(account => println(s"Account ${account.accountNumber}: ${account.balance}"))

    val totalBalance = calculateTotalBalance(bank)
    println(s"Total balance of all accounts: $totalBalance")

    val finalBalances = calculateFinalBalances(bank)
    println("Final balances after applying interest:")
    finalBalances.foreach(account => println(s"Account ${account.accountNumber}: ${account.balance}"))
  }


}
