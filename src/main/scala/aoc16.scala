package aoc2020

import scala.io.Source

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


case class TicketRules(name: String, firstRule: Range, secondRule: Range)

object AOC16 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc16.txt").mkString(sep =  "").split("\n\n").toList

  val testInput = Source.fromFile("src/main/resources/testinput.txt").mkString(sep =  "").split("\n\n").toList

  val (rules, myTicket, nearbyTickets) = input match
    case List(a, b, c) => (toTicketRules(a), toTicket(b), toTicket(c))

  val validTickets = nearbyTickets.filterNot(x => x.complyWithRule(rules))

  println("Answer part 1 = " + nearbyTickets.map(x => x.doesNotComplyWithRule(rules)).flatten.sum)

