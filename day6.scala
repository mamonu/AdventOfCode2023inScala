import scala.io.Source
import scala.collection.immutable._
import scala.util.matching.Regex

object day6 {

  def main(args: Array[String]): Unit = {


case class Race(time: Long, distance: Long) {
    def possibilities: Int = {
      Range.Long(0, time, 1).count { 
        speed => (time - speed) * speed > distance
      }
    }
  }

      lazy val input = Source.
                       fromResource("inputs/input6.txt").
                       getLines().
                       toStream

      lazy val inputPart1 = {
      val timePattern = new Regex("Time:\\s+(.*)")
      val distancePattern = new Regex("Distance:\\s+(.*)")

      val timeLine = input.collectFirst {
        case timePattern(numbers) => numbers
      }.getOrElse("")

      val distanceLine = input.collectFirst {
        case distancePattern(numbers) => numbers
      }.getOrElse("")

      val times = timeLine.
                  split("\\s+").
                  map(_.toLong)

      val distances = distanceLine.
                      split("\\s+").
                      map(_.toLong)

      times.zip(distances).map { case (t, d) => Race(t, d) }
    }

    lazy val inputPart2 = {
      val time = inputPart1.
                  map(_.time.toString).
                  mkString.
                  toLong

      val dist = inputPart1.
                  map(_.distance.toString).
                  mkString.
                  toLong

      Race(time, dist)
    }

    val part1 = inputPart1.
                map(_.possibilities).
                product
    val part2 = inputPart2.possibilities

 

    println("\nDay 6 solutions \n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2\n")
  }
}

