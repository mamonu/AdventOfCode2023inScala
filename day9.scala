import scala.io.Source
import scala.collection.immutable._
import scala.util.matching.Regex

object day9 {
  def main(args: Array[String]): Unit = {

    val input = Source.fromResource("inputs/input9.txt")
      .getLines
      .toStream
      .map(_.split("\\s+").map(_.toLong).toList)

    def extrapolate(history: List[Long]): (Long, Long) = {
      var step = history
      var all  = List.empty[List[Long]]
      while (step.exists(_ != 0)) {
        all = step :: all
        step = step.zip(step.tail).map { case (x, y) => y - x } 
      }

      all.foldLeft((0L, 0L)) { case ((exFirst, exLast), line) =>
        val first = line.head - exFirst
        val last  = line.last + exLast
        (first, last)
      }
    }

    val (part2, part1) = input.map(extrapolate).foldLeft((0L, 0L)) { case ((fs, ls), (first, last)) =>
      (fs + first, ls + last)
    }

    println("\nDay 9 solutions \n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2\n")
  }
}


