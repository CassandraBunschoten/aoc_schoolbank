package aoc2020

import scala.util.matching.Regex
import scala.io.Source

case class Bag(color: String, inner: List[(Int, String)])


def constructInner(input: String): List[(Int, String)] =
    input.split(",").toList.map(s => 
      val t =  raw"\d+".r.findFirstIn(s).getOrElse("0")
      val x = raw" ?\d ".r.replaceAllIn(s, "")
      (t.toInt, raw" bags?.*".r.replaceAllIn(x,"")))

def depthFirstSearch(all: List[Bag], color: String): Int =
  val Some(x) = all.find(_.color == color)
  x.inner
    .map((i, s) => i match
      case 0 => 0
      case t => t + t * depthFirstSearch(all,s))
    .sum

object AOC722020 extends App:
  
  val bagRules = Source
      .fromFile("src/main/resources/input_aoc7.txt")
      .getLines
      .map(_.split(" bags contain ").toList)
      .map(ib => 
        Bag(ib(0), constructInner(ib(1)))).toList

  // println(bagRules(118))
  println(depthFirstSearch(bagRules, "shiny gold"))
