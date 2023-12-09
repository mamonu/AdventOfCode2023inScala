import scala.io.Source
import scala.collection.immutable._
import scala.util.matching.Regex

object day4 {

  def main(args: Array[String]): Unit = {

    type CardNumber = Int

    case class Card(
      id: CardNumber,
      winning: List[Int],
      guesses: List[Int]
    )

    val CardRegex = """Card\s+(\d+)\s*:([^|]+)[|]([^$]+)$""".r

    val input = Source.fromResource("inputs/input4.txt")
      .getLines
      .map(_.trim)
      .filter(_.nonEmpty)
      .toStream

    val cards = input.flatMap {
      case CardRegex(nrStr, winningStr, guessesStr) =>
        val winning = winningStr.trim.split("\\s+").map(_.toInt).toList
        val guesses = guessesStr.trim.split("\\s+").map(_.toInt).toList
        Some(Card(nrStr.toInt, winning, guesses))
      case _ => None
    }

    val part1 = cards.map { card =>
      card.guesses.foldLeft(0)((acc, e) =>
        if (card.winning.contains(e))
          if (acc == 0) 1 else acc * 2
        else
          acc
      )
    }.sum

    val part2 = cards.foldLeft(Map.empty[CardNumber, Int]) { (map, card) =>
      val count = card.guesses.intersect(card.winning).size
      val wonThis = map.getOrElse(card.id, 0) + 1
      val newMap0 = map.updated(card.id, wonThis)
      cards.drop(card.id).take(count).foldLeft(newMap0) { (map, c) =>
        map.updated(c.id, map.getOrElse(c.id, 0) + wonThis)
      }
    }.values.sum

    println("\nDay 4 solutions \n------------")
    println(s"Part 1: $part1")
    println(s"Part 2: $part2\n")
  }
}

