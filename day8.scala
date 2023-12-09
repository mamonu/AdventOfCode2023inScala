import scala.io.Source
import scala.collection.immutable._
import scala.util.matching.Regex

object day8 {
  def main(args: Array[String]): Unit = {
    lazy val input = Source.fromResource("inputs/input8.txt").getLines().toList

    lazy val path = input.head.split("").toList

    val nodePattern = new Regex("(.+) = \\((.+), (.+)\\)")

    lazy val nodes = input.drop(2).foldLeft(Map.empty[String, (String, String)]) {
      case (acc, nodePattern(key, left, right)) => acc + (key -> (left, right))
      case (acc, _) => acc
    }

                                                                    

    def findPath(start: String, exit: String => Boolean): Long = {
      var cursor = start
      var count = 0L
      var currentPath = path

      while (!exit(cursor)) {
        val (left, right) = nodes(cursor)
        currentPath.head match {
          case "L" => cursor = left
          case "R" => cursor = right
        }

        currentPath = currentPath.tail
        if (currentPath.isEmpty) {currentPath = path}
        count += 1
                                }
        count
                                                                    }

    def lowestcommonmultiplier(a: BigInt, b: BigInt): BigInt = (a * b).abs / a.gcd(b)

    val part1 = findPath("AAA", _ == "ZZZ")

    val part2 = nodes.keys.filter(_.endsWith("A")).map { key =>
      BigInt(findPath(key, _.endsWith("Z")))
    }.reduce(lowestcommonmultiplier(_, _))

    println("\nDay 8 solutions \n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2\n")
  }
}


