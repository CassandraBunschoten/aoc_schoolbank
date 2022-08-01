package aoc2020

import scala.io.Source
import scala.collection.mutable

def run(startMap: mutable.Map[Int, (Int, Int)], start: Int, end: Int = 2020): Int =

  val startCounter = startMap.values.map(x => x._1).max

  def oneLine(updateMap: mutable.Map[Int, (Int, Int)], k: Int, counter: Int): Int =

    if (counter == startCounter)  oneLine(updateMap += (k -> (counter, counter)), 0, counter + 1) else {
      if (counter == end) k else {

        val gotValue = updateMap.getOrElse(k, (0, 0))

        gotValue match
          case (0, 0) => oneLine(updateMap += (k -> (counter, counter)), 0, counter + 1)
          case (x, _) => oneLine(updateMap += (k -> (counter, x)), (counter - x), counter + 1)
      }
    }

  oneLine(startMap, start, startCounter)


object aoc15 extends App:

  val startInput = Source.fromFile("src/main/resources/input_aoc15.txt")
                            .mkString
                            .split(",")
                            .toList
                            .map(_.toInt)

  val startNumbers = startInput
                            .zipWithIndex
                            .map{case (x, y) => (x, (y + 1, -1))}
                            .to(collection.mutable.Map)

  val startVal    = startInput.last

  println(run(startNumbers, startVal))
  println(run(startNumbers, startVal, 30000000))


