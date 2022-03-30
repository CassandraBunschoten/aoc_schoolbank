package aoc2020

import scala.util.matching.Regex
import scala.io.Source


// case class InBag(num: Int, color: String)
// case class RulesString(out: String, in: List[String])
// case class RulesIn(out: String, in: List[InBag]):
//   def trim: Rules =
//     val in2 = this.in.map(s => 
//       val x = raw" ?\d ".r.replaceAllIn(s, "")
//       raw" bags?.*".r.replaceAllIn(x,""))
//     Rules(out, in2)

case class Rules(out: String, in: List[String]):
  def trim: Rules =
    val in2 = this.in.map(s => 
      val x = raw" ?\d ".r.replaceAllIn(s, "")
      raw" bags?.*".r.replaceAllIn(x,""))
    Rules(out, in2)

def containsBag(l: List[Rules])(inner: String): List[String] = 
  l.filter(_.in.contains(inner)).map(_.out)

def bier(all: List[Rules], solve: List[String], current: String, done: List[String]): List[String] = 
  val newDone = current :: done
  val add = containsBag(all)(current).filter(!newDone.contains(_))
  val newSolve = solve ::: add.filter(!solve.contains(_))
  newSolve match 
    case Nil => current :: done
    case _   => bier(all, newSolve.drop(1), newSolve.head, newDone)

object AOC72020 extends App:

  val bagRules = Source
    .fromFile("fps/src/main/resources/input_aoc7.txt")
    .getLines
    .map(_.split(" bags contain ").toList)
    .map(ib => 
      Rules(ib(0),ib(1).split(",").toList))
    .map(_.trim).toList
    
  val goldContainers = bagRules.filter(_.in.contains("shiny gold"))
    .map(_.out)
    .toList
  
  val bagContainer = containsBag(bagRules)
  println(bier(bagRules, List(), "shiny gold", List()).length)