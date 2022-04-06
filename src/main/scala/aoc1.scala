package aoc2020 

import scala.io.Source

object AOC12020 extends App:
    val expenses = Source
        .fromFile("src/main/resources/input_aoc1.txt").getLines.map(_.toInt).toList

    def comboTwo: (Int, Int, Int) = 
      (for (
        a <- expenses;
        b <- expenses
        if (a + b == 2020)
    ) yield(a, b, a * b)).headOption.getOrElse(sys.error("Not found"))
      

    def comboThree: (Int, Int, Int, Int) =
      (for (
        a <- expenses;
        b <- expenses;
        c <- expenses
        if (a + b + c == 2020)
      ) yield(a, b, c, a * b * c)).headOption.getOrElse(sys.error("Not found"))

    println(comboTwo)
    println(comboThree)


