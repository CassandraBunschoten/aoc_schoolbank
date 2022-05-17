package aoc2020

import scala.io.Source
import scala.annotation.tailrec

case class Program(mask: BitString, mem: List[Memory]):
  def execute: List[Memory] =
    mem.map(x => x.copy(value = mask.combine(x.value)))

  def executePart2: List[Memory] = 
    mem.flatMap(x => mask.combine(x.addressToBitString.address).map(Memory(_, x.value)))

case class BitString(bitString: String):
  assert(bitString.length == 36)
  
  def combine(other: BitString): BitString =
    BitString(bitString.zip(other.bitString).map(cc => 
      cc match 
        case ('X', v) =>  v 
        case ('1', _) => '1'
        case ('0', _) => '0'
        ).mkString)

  def combine(other: String): List[String] =
    assert(other.length == 36)
    def rec(acc: String, tail: List[Char]): List[String] =
      tail match
       case Nil      => List(acc)
       case '0' :: t => rec(acc + '0', t)
       case '1' :: t => rec(acc + '1', t)
       case 'X' :: t => rec(acc + '0', t) ::: rec(acc + '1', t)
       case _   :: t => sys.error("this should not happen")
       
    val bs = bitString.zip(other).map(cc =>
      cc match
        case ('X', _) => 'X'
        case ('1', _) => '1'
        case ('0', v) =>  v
        ).toList
    rec("", bs)

  def toLong: Long =
    bitString.foldRight((0L, 0)){case (elm, (acc, counter)) => 
      elm match
        case '0' => (acc, counter + 1)
        case '1' => (acc + (Math.pow(2, counter).toLong), counter + 1)
        case _   => sys.error("crash")
    }._1


case class Memory(address: String, value: BitString):
  def addressToBitString: Memory = 
    copy(address = leftpad(address.drop(4).dropRight(1).toLong))

def leftpad(i: Long, digits: Int = 36): String =
    String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0')

def stringToProgram(input: String): Program = 
  val inputls = input.split("\n")
  val mask = BitString(inputls.head)

  val memory  = inputls.tail.map(s => 
    val array = s.split(" = ")
    Memory(array(0), BitString(leftpad(array(1).toLong)))).toList

  Program(mask, memory)



object aoc142020 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc14.txt")
                    .mkString
                    .split("mask = ")
                    .toList
                    .tail
                    .map(stringToProgram(_))
  
  val mapped = input.flatMap(_.execute).map(x => (x.address, x.value)).toMap 
  val mapped2 = input.flatMap(_.executePart2).map(x => (x.address, x.value)).toMap 
  val mem = Memory("mem[42]",BitString("000000000010010000110111111111101111"))
  // println(BitString("000000000000000000000000000000X1001X").combine(mem.addressToBitString.address))
  // println(mem.addressToBitString)
  // println(mapped.values.map(_.toLong).sum)
  println(mapped2.values.map(_.toLong).sum)

                    
