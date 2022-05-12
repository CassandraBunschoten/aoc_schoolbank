package aoc2020

import scala.io.Source

sealed trait NavInstructions(value: Int)
case class North(value: Int)     extends NavInstructions(value)
case class South(value: Int)     extends NavInstructions(value)
case class East(value: Int)      extends NavInstructions(value)
case class West(value: Int)      extends NavInstructions(value)
case class Left(value: Int)      extends NavInstructions(value)
case class Right(value: Int)     extends NavInstructions(value)
case class Forward(value: Int)   extends NavInstructions(value)

def stringToNI(input: String): NavInstructions = 
  input.head match
    case 'N' => North(input.tail.toInt)
    case 'S' => South(input.tail.toInt)
    case 'E' => East(input.tail.toInt)
    case 'W' => West(input.tail.toInt)
    case 'L' => Left(input.tail.toInt)
    case 'R' => Right(input.tail.toInt)
    case 'F' => Forward(input.tail.toInt)

case class WayPointState(shipX: Int, shipY: Int, wayX: Int, wayY: Int):
  def nextState(ni: NavInstructions): WayPointState = 
    ni match
      case North(v)                   => copy(wayY = wayY + v)
      case South(v)                   => copy(wayY = wayY - v)
      case East(v)                    => copy(wayX = wayX + v)
      case West(v)                    => copy(wayX = wayX - v)
      case (Left(90)  | Right(270))   => copy(wayX = -wayY, wayY = wayX)
      case (Left(270) | Right(90))    => copy(wayX = wayY, wayY = -wayX)
      case (Left(180) | Right(180))   => copy(wayX = 0 - wayX, wayY = 0 - wayY)
      case Forward(v)                 => copy(shipX = shipX + v * wayX, shipY = shipY + v * wayY)
      case _                          => sys.error("booboo")
      

case class State(x: Int, y: Int, d: Int):
  def nextState(ni: NavInstructions): State = 
    ni match
      case North(v)   => copy(y = y + v)
      case South(v)   => copy(y = y - v)
      case East(v)    => copy(x = x + v)
      case West(v)    => copy(x = x - v)
      case Left(v)    => copy(d = (360 + (d - v)) % 360)
      case Right(v)   => copy(d = (360 + (d + v)) % 360)
      case Forward(v) => d match
        case (0)   => copy(y = y + v)
        case (90)  => copy(x = x + v)
        case (180) => copy(y = y - v)
        case (270) => copy(x = x - v)

object aoc122020 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc12.txt")
    .getLines
    .map(stringToNI(_))
    .foldLeft(State(0,0,90))((acc, elm) => acc.nextState(elm))
  
  println(Math.abs(input.x) + Math.abs(input.y))

  val input2 = Source.fromFile("src/main/resources/input_aoc12.txt")
    .getLines
    .map(stringToNI(_))
    .foldLeft(WayPointState(0, 0, 10, 1))((acc, elm) => acc.nextState(elm))

  println(Math.abs(input2.shipX) + Math.abs(input2.shipY))