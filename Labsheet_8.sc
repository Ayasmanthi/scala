import scala.io.StdIn

object Labsheet08 {

  def main(args: Array[String]): Unit = {
    //Question 01
    println("----------------Calculating the interest---------------")
    //val depositAmount: Double = 250000
    println("Enter the deposited amount (in Rs.):")
    val depositAmount: Double = StdIn.readDouble()

    val interestAmount: Double = calculateInterest(depositAmount)
    println("Interest Amount: Rs." + interestAmount)

    //Question 02
    println("-------------Pattern Matching----------------")
    println("Enter an integer:");
    val input: Int = StdIn.readInt();

    println("Output :" + classifyNumber(input))

    //Question 03
    print("-----------Format Names-----------------\n")
    println(toUpper("Benny"));
    println(formatNames("Niroshan", 1));
    println(toLower("Saman"));
    println(formatNames("Kumara", 5));
  }

  //Function 01
  def calculateInterest: Double => Double = (depositAmount: Double) => { //Lambda Function
    //This is equivalent to defining a regular function, but it's assigned to a value directly.
    val interestRate: Double = depositAmount match {
      case amount if amount <= 20000 => 0.02
      case amount if amount <= 200000 => 0.04
      case amount if amount <= 2000000 => 0.035
      case _ => 0.065
    }

    depositAmount * interestRate
  }

  //Function 02
  val classifyNumber: Int => String = (num: Int) => num match {
    case n if n <= 0 => "Negative/Zero is input"
    case n if n % 2 == 0 => "Even number is given"
    case _ => "Odd number is given"
  }

  //Function 03
  val toUpper: String => String = (input: String) => input.toUpperCase()

  val toLower: String => String = (input: String) => input.toLowerCase()

  val formatNames: (String, Int) => String = (word: String, index: Int) => {
    val wordChar = word.charAt(index)
    val modified = wordChar.toUpper
    word.patch(index, modified.toString, 1)
  }
}