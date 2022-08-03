package aoc2020

import scala.io.Source

case class TicketRules(name: FieldName, firstRule: Range, secondRule: Range)

type FieldName = String

def toTicket(input: String): List[Ticket] =
  val tickets = input.split("\n").map(_.split(",").toList).toList
  val intTickets = tickets.map(x => x.map(_.toIntOption).flatten)
  intTickets.filterNot(x => x.isEmpty).map(x => Ticket(x))


def toTicketRules(input: String): List[TicketRules] =
  def defineRuleRange(rule: String): Range =
    val ruleRange = rule.split("-").map(_.toInt)
    Range.inclusive(ruleRange(0), ruleRange(1), 1)

  val ticketRules = input.split("\n").toList.map(_.split(": ").toList)
  ticketRules.map(x => x match
    case List(name, rules) => rules.split(" or ").toList match
      case first :: second => val range1 =  defineRuleRange(first)
        val range2 = defineRuleRange(second.head)
          TicketRules(name, range1, range2)
  )


case class Ticket(numbers: List[Int]):
  def doesNotComplyWithRule(rules: List[TicketRules]) =
    def boolComply(rules: List[TicketRules], numCheck: Int): Boolean =
      rules match
        case h :: t => (h.firstRule.contains(numCheck) || h.secondRule.contains(numCheck)) || boolComply(t, numCheck)
        case Nil    => false

    this.numbers.filterNot(x => boolComply(rules, x))

  def complyWithRule(rules: List[TicketRules]): Boolean =
    def boolComply(rules: List[TicketRules], numCheck: Int): Boolean =
      rules match
        case h :: t => (h.firstRule.contains(numCheck) || h.secondRule.contains(numCheck)) || boolComply(t, numCheck)
        case Nil    => false

    this.numbers.map(x => boolComply(rules, x)).contains(false)

  def correctRulesForField(rules: List[TicketRules]): List[List[FieldName]] =
    def recFilterFieldNames(rules: List[TicketRules], numCheck: Int): List[FieldName] =
      rules match
        case h :: t => if (h.firstRule.contains(numCheck) || h.secondRule.contains(numCheck))
                        h.name :: recFilterFieldNames(t, numCheck) else "" :: recFilterFieldNames(t, numCheck)
        case Nil    => Nil

    this.numbers.map(x => recFilterFieldNames(rules, x))


def checkPerTicketLocation(fieldNames: List[List[List[FieldName]]]) =
  val numLocations = fieldNames.length
  val names = fieldNames.flatten.flatten.distinct.filterNot(_ == "")
  val numOccurrences = fieldNames.transpose.map(x => x.flatten.filterNot(_ == "").zipWithIndex.groupBy(_._1).transform((_, v) => v.length).toList).zipWithIndex

  def findSolutions(input: List[(List[(FieldName, Int)], Int)], remainingNames: List[FieldName], accResult: List[(FieldName, Int)]): List[(FieldName, Int)] =

    val result = findOneUniqueSolution(input)

    val acc = result :: accResult

    val newNames = remainingNames.filterNot(_ == result._1)

    if (newNames.length == 0) acc else
      findSolutions(input.filterNot(x => x._2 == result._2).map{case (x, y) => (x.filterNot(z => z._1 == result._1), y)}, newNames, acc)

  def findOneUniqueSolution(input: List[(List[(FieldName, Int)], Int)]): (FieldName, Int)  =
    input match
      case h :: t => val check = h._1.filter(x => x._2 == numLocations) ; if (check.length == 1) (check.head._1, h._2) else
        findOneUniqueSolution(t)
      case Nil    => sys.error("found nothing")

  findSolutions(numOccurrences, names, List.empty: List[(FieldName, Int)])


object AOC16 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc16.txt").mkString(sep =  "").split("\n\n").toList

  val testInput = Source.fromFile("src/main/resources/testinput.txt").mkString(sep =  "").split("\n\n").toList

  val (rules, myTicket, nearbyTickets) = input match
    case List(a, b, c) => (toTicketRules(a), toTicket(b), toTicket(c))
    case _             => sys.error("incorrect input")

  println("Answer part 1 = " + nearbyTickets.map(x => x.doesNotComplyWithRule(rules)).flatten.sum)

  val validTickets = nearbyTickets.filterNot(x => x.complyWithRule(rules))

  val intermediateSave = validTickets.map(x => x.correctRulesForField(rules))

  val positions = checkPerTicketLocation(intermediateSave).filter{case (a, _) => a.startsWith("departure")}.map(x => x._2)

  val fieldsOwnTicket = myTicket.head.numbers.zipWithIndex.filter{x => positions.contains(x._2)}

  println("Answer part 2 = " + fieldsOwnTicket.map(x => x._1.toLong).foldRight(1: Long)(_ * _))


