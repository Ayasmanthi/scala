import scala.io.StdIn._

object labsheet_7 {

    def main(args: Array[String]): Unit = {

      //Q1
      print("Enter number list: ")
      val numlist1 = readLine().split(" ").map(_.toInt).toList
      println("Output : " + filterEvenNumbers(numlist1))

      //Q2
      print("Enter number list: ")
      val numlist2 = readLine().split(" ").map(_.toInt).toList
      println("Output : " + calculateSquare(numlist2))

      //Q3
      print("Enter number list: ")
      val numlist3 = readLine().split(" ").map(_.toInt).toList.filter(_ > 1)
      println(filterPrime(numlist3))
    }

    //Q1
    def filterEvenNumbers(numbers: List[Int]): List[Int] = {
      numbers.filter(number => number % 2 == 0)//filter numbers only divide by 2
    }

    //Q2
    def calculateSquare(numbers: List[Int]): List[Int] = {
      numbers.map(number => number * number)//map the square number with that number
    }

  //Q3
  def gcd(p: Int, q: Int): Int = p match {
    case x if (x < q) => gcd(q, x)
    case x if (x % q == 0) => q
    case x => gcd(q, x % q)
  }

  def isPrime(n: Int, i: Int = 2): Boolean = n match {
    case x if (x == i) => true//if iteration of gcd function, is 1 then it is a prime
    case x if (gcd(x, i) > 1) => false
    case x => isPrime(x, i + 1)
  }

  def filterPrime(numbers: List[Int]): List[Int] = {
    numbers.filter(num => isPrime(num))
  }


}
