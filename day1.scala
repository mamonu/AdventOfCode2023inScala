import scala.io.Source
import scala.collection.immutable._
import scala.util.matching.Regex

object day1
{

def main(args:Array[String]):Unit ={
val inputFile = Source.fromFile("inputs/input1.txt")
val lines = inputFile.getLines().toList
inputFile.close()


val digitMap: Map[String, Int] = Map(
    "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5,
    "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9
  )

def extractCalibrationValue_1a(line: String): Int = {
  val digits = line.filter(_.isDigit)
  digits.length match {
    case 0 => 0 // No digits in the line
    case 1 => (digits + digits).toInt 
    case _ => (digits.head.toString + digits.last.toString).toInt
  }
}


  def extractCalibrationValue_1b(line: String): Int = {
    val replacedLine = digitMap.foldLeft(line) {
      case (acc, (word, num)) =>
        val wordPattern: Regex = s"\\b$word\\b".r
        wordPattern.replaceAllIn(acc, num.toString)
    }
  
    val digits = replacedLine.filter(_.isDigit)
    digits.length match {
      case 0 => 0 // No digits in the line
      case 1 => (digits + digits).toInt // Only one digit, duplicate it
      case _ => (digits.head.toString + digits.last.toString).toInt // Two or more digits
    }
  
  

  }


 println("\nDay 1 solutions \n------------")

val calValues_1 = lines.map(extractCalibrationValue_1a)
val totalCal_1 = calValues_1.sum
println(s"Total Calibration Value for 1st problem: $totalCal_1")

// not working correctly :(
val calValues_2 = lines.map(extractCalibrationValue_1b)
val totalCal_2 = calValues_2.sum
println(s"Total Calibration Value for 2nd problem: $totalCal_2")



} 
}