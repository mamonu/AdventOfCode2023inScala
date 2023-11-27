import scala.io.Source
import scala.collection.immutable._


object day1
{

def main(args:Array[String]):Unit ={
val inputFile = Source.fromFile("inputs/01-input.txt")

def loadInput(inputText: String): List[List[Int]] = {
  inputText.split("\n\n").map(_.split("\n").map(_.toInt).toList).toList
}



    
}
