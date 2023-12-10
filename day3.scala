import scala.io.Source
import scala.collection.immutable._
import scala.util.matching.Regex

object day3 {

def main(args: Array[String]): Unit = {

def isSpecialChar(ch: Char): Boolean =
    (ch < '0' || ch > '9') && ch != '.'

def adjacentCoordinates(i: Int, j: Int): List[(Int, Int)] =
    List(
      (i, j + 1),(i, j - 1),
      (i + 1, j),(i + 1, j - 1),
      (i + 1, j + 1),
      (i - 1, j),(i - 1, j - 1),
      (i - 1, j + 1)
    )
    

val matrix = Source.fromResource("inputs/input3.txt")
      .getLines
      .map(_.trim)
      .filter(_.nonEmpty)
      .toVector
      .map(_.toVector)

var allNumbers = Vector.empty[Int]
var allGears = Map.empty[(Int, Int), List[Int]]


    for (i <- matrix.indices) {
      var isInNumber = false
      var isAdjacentSpecial = false
      var detectedGears = Set.empty[(Int, Int)]
      var partialNumber = 0

      for (j <- matrix(i).indices) {
        matrix(i).lift(j) match {
          case Some(ch) if ch.isDigit =>
            isInNumber = true
            partialNumber = partialNumber * 10 + (ch - '0')

            adjacentCoordinates(i, j).foreach {
              case (x, y) if matrix.isDefinedAt(x) && matrix(x).isDefinedAt(y) =>
                val ch2 = matrix(x)(y)
                if (isSpecialChar(ch2)) {
                  isAdjacentSpecial = true
                  if (ch2 == '*') detectedGears += ((x, y))
                }
              case _ => ()
            }

          case Some(_) if isInNumber =>
            if (isAdjacentSpecial) allNumbers = allNumbers :+ partialNumber
            detectedGears.foreach { gear =>
              allGears = allGears.updated(
                gear,
                partialNumber :: allGears.getOrElse(gear, Nil)
              )
            }
            isInNumber = false
            detectedGears = Set.empty
            isAdjacentSpecial = false
            partialNumber = 0

          case _ => ()
        }
      }

      // Process any remaining number at the end of a row
      if (isInNumber) {
        if (isAdjacentSpecial) allNumbers = allNumbers :+ partialNumber
        detectedGears.foreach { gear =>
          allGears = allGears.updated(
            gear,
            partialNumber :: allGears.getOrElse(gear, Nil)
          )
        }
      }
    }

    val part2 = allGears.values
      .filter(_.size >= 2)
      .map(_.product.toLong).sum
      

 val part1 = allNumbers.sum

    println("\nDay 3 solutions \n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2\n")
  }
}
