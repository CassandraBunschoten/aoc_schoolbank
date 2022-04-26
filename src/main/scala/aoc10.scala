package aoc2020

import scala.io.Source

sealed trait Num

case object One extends Num
case object Two extends Num
case object Three extends Num

// def calculate(slide: List[Int]): Int =
//   for(
//     j <- slide.tail
//     i  = slide.head
//   )


object AOC102020 extends App: 
  val input = Source
    .fromFile("src/main/resources/input_aoc10.txt")
    .getLines
    .map(_.toInt)
    .toList
    .sorted

  val mid = 0 :: input ::: List(input.reverse.head + 3)


  val answer = mid.sliding(2)
    .map(x =>
      val t = x(0)
      x(1)-x(0) match
        case 1 => One
        case 2 => Two
        case 3 => Three
        case _ => throw(sys.error("help"))
    )
    .toList
    
  println(mid)
  println(answer.count(_ == One) *  (answer.count(_ == Three)))