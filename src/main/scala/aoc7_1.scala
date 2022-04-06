package aoc2020

import scala.util.matching.Regex
import scala.io.Source
import cats.data.State 

case class Rules(out: String, in: List[String]):
  def trim: Rules =
    val in2 = this.in.map(s => 
      val x = raw" ?\d ".r.replaceAllIn(s, "")
      raw" bags?.*".r.replaceAllIn(x,""))
    Rules(out, in2)

case class Status(toSolve: List[String], done: List[String])

def containsBag(l: List[Rules])(inner: String): List[String] = 
  l.filter(_.in.contains(inner)).map(_.out)

def breadthFirstSearch(all: List[Rules], solve: List[String], current: String, done: List[String]): List[String] =
  val newDone = current :: done
  val add = containsBag(all)(current).filter(!newDone.contains(_))
  val newSolve = solve ::: add.filter(!solve.contains(_))
  newSolve match
    case Nil => current :: done
    case _   => breadthFirstSearch(all, newSolve.drop(1), newSolve.head, newDone)

def breadthStatus(all: List[Rules]): State[Status, Int] =
  State(s => 
    val current = s.toSolve.head
    val newDone = current :: s.done
    val newSolve = s.toSolve.drop(1) ::: containsBag(all)(current)
               .filter(!newDone.contains(_))
               .filter(!s.toSolve.contains(_))
    newSolve match
      case Nil => (Status(newSolve, newDone), newDone.length)
      case _   => breadthStatus(all).run(Status(newSolve, newDone)).value
  )

object AOC72020 extends App:

  val bagRules = Source
    .fromFile("src/main/resources/input_aoc7.txt")
    .getLines
    .map(_.split(" bags contain ").toList)
    .map(ib => 
      Rules(ib(0),ib(1).split(",").toList))
    .map(_.trim).toList
    
  val goldContainers = bagRules.filter(_.in.contains("shiny gold"))
    .map(_.out)
    .toList
  
  val bagContainer = containsBag(bagRules)
  println(breadthFirstSearch(bagRules, List(), "shiny gold", List()).length)
  val out = breadthStatus(bagRules).runA(Status(List("shiny gold"), List.empty)).value
  println(out)
  