import  scala.io.StdIn //inorder to get user inputs

object labsheet_4 {
  def main(args: Array[String]) = {


    println("Enter amount of the deposit: ")
    val deposit = StdIn.readInt()
    println("The interest for the deposit is =" + calculate_interest(deposit))
    println("Enter a integer number: ")
    val n = StdIn.readInt()
    Q2(n)

    println("Enter a string:")
    val str = StdIn.readLine()

    val upperString = formatNames(str, _.toUpperCase)
    println(upperString)

    val lowerString = formatNames(str, _.toLowerCase)
    println(lowerString)

    val names = List("Benny", "NIroshan", "Saman", "Kumara")

    val formattedString1 = formatNames(names(0), _.toUpperCase)
    println(formattedString1)

    val formattedString2 = formatNames(names(1), capitalizeNAndI)
    println(formattedString2)

    val formattedString3 = formatNames(names(2), _.toLowerCase)
    println(formattedString3)

    val formattedString4 = formatNames(names(3), s => s.init + s.last.toUpper)
    println(formattedString4)

  }

  //Q1
  def calculate_interest(amount: Int): Double = {
    var rate = 0.00
    if (amount <= 20000) {
      rate = 0.02
    }
    else if (amount <= 200000) {
      rate = 0.04
    }
    else if (amount <= 2000000) {
      rate = 0.035
    }
    else {
      rate = 0.065
    }

    var interest_amount = (amount) * (rate)
    return interest_amount
  }


  //Q2
  def Q2(num: Int) = {
    if (num <= 0) {
      println("Negative/Zero")
    }
    else {
      if (num % 2 == 0) {
        println("Even")

      }
      else {
        println("Odd")
      }
    }

  }

  //Q3
  def formatNames(name: String, formatFunc: String => String): String = {
    formatFunc(name)
  }

  def capitalizeNAndI(str: String): String = {
    str.map { c =>
      if (c.toLower == 'n' || c.toLower == 'i') c.toUpper
      else c

    }
  }
}
