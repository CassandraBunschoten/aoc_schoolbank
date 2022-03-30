package aoc2020
import scala.io.Source

def move(l: List[(String, Int)], right: Int): Long = 
  l.map(_ match 
    case (string, int) => string.charAt((int * right) % string.length) == '#')
    .count(_ == true)
    .toLong

object AOC32020 extends App:

    val input = Source
      .fromFile("fps/src/main/resources/input_aoc3.txt")
      .getLines
      .toList
      .zipWithIndex

      val move2 = 
        input.
          map(_ match
            case (string, int) => if (int % 2 == 0) 
              then string.charAt((int - (int /2)) % string.length) == '#' else false)
              .count(_ == true)
              .toLong

    println(s"${move(input, 1)} * ${move(input, 3)} * ${move(input, 5)} * ${move(input, 7)} * $move2")
    println(move(input, 1) * move(input, 3)* move(input, 5) * move(input, 7) * move2)