package aoc2020

import scala.io.Source
import scala.collection.immutable.*

sealed trait Num
case object One extends Num
case object Two extends Num
case object Three extends Num

case class   IntNode(int: Int, branches: List[Int])

sealed trait Tree
case class   Node(int: Int, leaf: Long) extends Tree
case object  Leaf extends Tree

def makeNodes(lis: List[Int]): IntNode =
  val i = lis.head
  val b = lis.drop(1).filter(x => x == i + 1 || x ==  i + 2 || x == i + 3)
  IntNode(i, b)

def makeTree(lis: List[IntNode]): Long =
  val seen = HashMap((lis.reverse.head.int, 1L))
  val done = lis.reverse.drop(1).foldLeft(seen)((acc, elm) => 
    val br = elm.branches.map(x => 
      acc.get(x) match
        case Some(x) => x
        case None    => sys.error(s"Ik kan hem niet vinden ${x}"))
    acc + ((elm.int, br.sum))
  )
  done.get(0) match
    case Some(x) => x
    case None    => sys.error("hoe heb je dit voor elkaar gekregen")

  

object AOC102020 extends App: 
  val input = Source
    .fromFile("src/main/resources/input_aoc10.txt")
    .getLines
    .map(_.toInt)
    .toList
    .sorted

  val mid = 0 :: input ::: List(input.reverse.head + 3)

  val answer = mid.sliding(2)
    .map(x =>
      val t = x(0)
      x(1)-x(0) match
        case 1 => One
        case 2 => Two
        case 3 => Three
        case _ => throw(sys.error("help"))
    )
    .toList

    
  val hackyMid = 0 :: input ::: List(input.reverse.head + 3, 0, 0)

  val nodes = hackyMid.sliding(4).map(makeNodes(_)).toList
  println(makeTree(nodes))
  println(answer.count(_ == One) * (answer.count(_ == Three)))