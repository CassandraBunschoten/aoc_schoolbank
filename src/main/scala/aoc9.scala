package aoc2020

import scala.io.Source

def check(ls: List[List[Long]], pre_length: Int): Long =
  
  val preamble = ls.head.take(pre_length)
  val target = ls.head.drop(pre_length).head

  if !(rec(preamble, preamble.tail, target)) then
    target
  else
    check(ls.drop(1), pre_length)

def rec(ls: List[Long], ls1: List[Long], target: Long): Boolean =
   ls.head + ls1.head == target match
     case true  => true
     case false => 
        ls1.tail match
          case Nil => 
            ls.tail.length > 1 match
              case false => false
              case true  => rec(ls.tail, ls.tail.tail, target)
          case t   => rec(ls, t, target)

def recCont(ls: List[Long], sliding: Int, target: Long): List[Long] =
  ls.sliding(sliding).toList.dropWhile(x => x.sum != target) match
    case Nil => recCont(ls, sliding + 1, target)
    case t   => t.head

object AOC92020 extends App: 
  val xmas = Source
    .fromFile("src/main/resources/input_aoc9.txt")
    .getLines
    .toList
    .map(_.toLong)
    
  val sliding  = xmas.sliding(26).toList
  val target = check(sliding, 25)
  val list = xmas.takeWhile(_ != target)
  val answer = recCont(list, 2, target)
  println(answer.min + answer.max)
