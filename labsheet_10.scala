object labsheet_10 {
  def main(args: Array[String]): Unit = {

    //Q01
    val temperaturesCelsius: List[Double] = List(0, 10, 20, 30)
    val averageFahrenheit = calculateAverage(temperaturesCelsius)
    println(s"Average Fahrenheit temperature: $averageFahrenheit")

    //Q02
    val words = List("apple", "banana", "cherry", "date")
    val totalCount = countLetterOccurrences(words)
    println(s"Total count of letter occurrences: $totalCount")
  }

  //Q01
  def calculateAverage(temperaturesCelsius: List[Double]): Double = {
    val temperaturesFahrenheit = temperaturesCelsius.map(c => (c * 9 / 5) + 32)
    val totalFahrenheit = temperaturesFahrenheit.reduce(_ + _)
    val averageFahrenheit = totalFahrenheit / temperaturesFahrenheit.length
    averageFahrenheit
  }

  //Q02
  def countLetterOccurrences(words: List[String]): Int = {
    val letterCounts = words.map(_.length)
    val totalLetterOccurrences = letterCounts.reduce(_ + _)
    totalLetterOccurrences
  }


}
