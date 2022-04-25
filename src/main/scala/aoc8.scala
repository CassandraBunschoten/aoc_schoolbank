package aoc2020

import scala.io.Source
import scala.util.matching.Regex

enum Operation(val underlying: String):
  case Nop extends Operation("nop")
  case Acc extends Operation("acc")
  case Jmp extends Operation("jmp")

case class Instructions(operation: Operation, argument: Int, visited: Boolean)

object Instructions:
  import Operation._
  def cons(input: String): Instructions =
    val Array(ope, arg) = input.split(' ')
    val x = ope match
      case Nop.underlying => Nop
      case Acc.underlying => Acc
      case Jmp.underlying => Jmp

    Instructions(x, arg.toInt, false)

def compute(index: Int, acc: Int, inst: List[Instructions]): Int = 
  import Operation._
   inst(index) match
        case Instructions(Nop, x, _) => 
          infinitesCheck(index + x, acc, inst.updated(index, Instructions(Jmp, x, true))) match
            case None => compute(index + 1, acc, inst.updated(index, Instructions(Nop, x, true)))
            case Some(x) => x
        case Instructions(Acc, x, _) => 
          compute(index + 1, acc + x, inst.updated(index, Instructions(Acc, x, true)))
        case Instructions(Jmp, x, _) => 
          infinitesCheck(index + 1, acc, inst.updated(index, Instructions(Nop, x, true))) match
            case None => compute(index + x, acc, inst.updated(index, Instructions(Jmp, x, true)))
            case Some(x) => x

def infinitesCheck(index: Int, acc: Int, inst: List[Instructions]): Option[Int] = 
  import Operation._

  if index >= inst.length then Some(acc)
  else
    inst(index) match
        case Instructions(_, _, true) => None
        case Instructions(Nop, _, _) => infinitesCheck(index + 1, acc, inst.updated(index, Instructions(Nop, 0, true)))
        case Instructions(Acc, x, _) => infinitesCheck(index + 1, acc + x, inst.updated(index, Instructions(Acc, x, true)))
        case Instructions(Jmp, x, _) => infinitesCheck(index + x, acc, inst.updated(index, Instructions(Jmp, x, true)))

object AOC82020 extends App:
  val bootCode = Source
    .fromFile("src/main/resources/input_aoc8.txt")
    .getLines
    .map(Instructions.cons(_))
    .toList

  println(compute(0, 0, bootCode))
