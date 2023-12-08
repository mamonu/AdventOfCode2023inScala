import scala.io.Source
import scala.collection.immutable._
import scala.util.matching.Regex

object day2 {

  def main(args: Array[String]): Unit = {

    case class GameSet(red: Int, green: Int, blue: Int) {
      def subsetOf(other: GameSet): Boolean =
        red <= other.red && green <= other.green && blue <= other.blue

      def power: Int =
        red * green * blue
    }

    case class Game(id: Int, sets: List[GameSet]) {
      def minimum: GameSet = GameSet(
        red = sets.map(_.red).max,
        green = sets.map(_.green).max,
        blue = sets.map(_.blue).max
      )
    }

    val initialBag = GameSet(red = 12, green = 13, blue = 14)


    def parseGameSet(gameSetStr: String): GameSet = {
    val ExtractCount = """^\s*(\d+)\s+(red|green|blue)\s*$""".r
    val map = gameSetStr.split("\\s*,\\s*")
      .map {
        case ExtractCount(nr, color) => (color, nr.toInt)
      }
      .toMap
    GameSet(
      red = map.getOrElse("red", 0),
      green = map.getOrElse("green", 0),
      blue = map.getOrElse("blue", 0)
    )
  }

    def parseGames(lines: Iterator[String]): List[Game] = {
    val ExtractGame = """^Game\s+(\d+)[:]([^$]+)$""".r
    lines.collect {
      case ExtractGame(id, setsStr) =>
        val sets = setsStr.split("\\s*;\\s*").map(parseGameSet).toList
        Game(id.toInt, sets)
    }.toList
  }

    val games = parseGames(Source.fromResource("inputs/input2.txt").getLines)


    val part1 = games
      .filter(_.sets.forall(_.subsetOf(initialBag)))
      .map(_.id)
      .sum

    val part2 = games.map(_.minimum.power).sum

    

    println("\nDay 2 solutions \n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2\n")
  }
}
