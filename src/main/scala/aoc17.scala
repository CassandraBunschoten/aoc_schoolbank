package aoc2020

import scala.io.Source

trait Conway
case object Active extends Conway
case object Inactive extends Conway

object Conway:
  def cons(char: Char): Conway =
    char match
      case '.' => Inactive
      case '#' => Active
      case _   => sys.error("what did you do?????")

case class Position(x: Int, y: Int, z: Int)

def getExtreme(pls: List[Position], f: (Int, Int) => Boolean): Position =
  def rec(pls: List[Position], x: Int, y: Int, z: Int): Position =
    pls match 
      case Nil => Position(x, y, z)
      case Position(hx,hy,hz) :: t if(f(hx, x) && f(hy, y) && f(hz, z)) => rec(t, hx, hy, hz)
      case Position(hx,hy,hz) :: t if(f(hy, y) && f(hz, z)) => rec(t, x, hy, hz)
      case Position(hx,hy,hz) :: t if(f(hx, x) && f(hz, z)) => rec(t, hx, y, hz)
      case Position(hx,hy,hz) :: t if(f(hx, x) && f(hy, y)) => rec(t, hx, hy, z)
      case Position(hx,hy,hz) :: t if(f(hx, x)) => rec(t, hx, y, z)
      case Position(hx,hy,hz) :: t if(f(hy, y)) => rec(t, x, hy, z)
      case Position(hx,hy,hz) :: t if(f(hz, z)) => rec(t, x, y, hz)
      case Position(hx,hy,hz) :: t => rec(t, x, y, z)
      case _ => sys.error("you failed")
  rec(pls, 0,0,0)


  
case class Grid(map: Map[Position, Conway])

object Grid:
  def cons(input: List[List[Conway]]): Grid =
    val is = for{
      i <- (0 to input.head.length-1)
      j <- (0 to input.length-1)
      conway = input(i)(j)
      position = Position(i, j, 0)
    } yield (position, conway)
    Grid(is.toMap)

object aoc172020 extends App:
  val input = Source.fromFile("src/main/resources/testinput.txt")
                    .getLines
                    .toList
                    .map(_.toCharArray.map(c => Conway.cons(c)).toList)
  

  val grid = Grid.cons(input)
  println(getExtreme(grid.map.keys.toList, (_ > _)))
  println(getExtreme(grid.map.keys.toList, (_ < _)))
