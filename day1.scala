import scala.io.Source
import scala.collection.immutable._
import scala.util.matching.Regex

object day1
{

def main(args:Array[String]):Unit ={


def part1: Int = {
  val regex: Regex = """\d""".r
  Source
    .fromResource("inputs/input1.txt")
    .getLines
    .flatMap { line =>
      val all = regex.findAllIn(line).map(_.toInt).toVector
      for {
        f <- all.headOption
        l <- all.lastOption
      } yield f * 10 + l
    }
    .sum
}


def part2: Int = {
val replacements: Map[String, String] = Map(
    "one" -> "one1one",
    "two" -> "two2two",
    "three" -> "three3three",
    "four" -> "four4four",
    "five" -> "five5five",
    "six" -> "six6six",
    "seven" -> "seven7seven",
    "eight" -> "eight8eight",
    "nine" -> "nine9nine"
  )
  val regex: Regex = """\d""".r
  Source
    .fromResource("inputs/input1.txt")
    .getLines
    .map { line =>
    val replacedLine = replacements.foldLeft(line)((acc, replacement) => acc.replaceAll(replacement._1, replacement._2))
    val allDigits: Vector[Int] = regex.findAllIn(replacedLine).map(_.toInt).toVector
      (allDigits.headOption, allDigits.lastOption)
    }
    .collect { case (Some(first:Int), Some(last:Int)) => first * 10 + last }
    .sum
}



 println("\nDay 1 solutions \n------------")

println(s"part 1: $part1")
println(s"part 2: $part2")





} 
}