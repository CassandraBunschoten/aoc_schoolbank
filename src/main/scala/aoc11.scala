package aoc2020

import scala.io.Source

sealed trait Cell:
  override def toString: String =
    this match
      case Occupied => "#"
      case Floor    => "."
      case Empty    => "L"
case object Occupied extends Cell
case object Floor    extends Cell
case object Empty    extends Cell

case class Pos(x: Int, y: Int):
  def addition(other: Pos): Pos =
    Pos(x + other.x, y + other.y)

case class Area51(cells: Map[Pos, Cell], maxX: Int, maxY: Int):

  override def toString: String =
    val t = for{
      i     <- (0 to maxX)
      cell   = (0 to maxY).toList.map(j => cells.get(Pos(j,i))).flatten
    } yield (cell.foldLeft("")((acc,elm) => acc + ' ' + elm.toString) + "\n")
    t.foldLeft("")((acc,elm) => acc + elm.toString)
 
 
  val offsets: List[Pos] =
    List(Pos(-1,-1), Pos(0,-1), Pos(1,-1), Pos(-1,0), Pos(1,0), Pos(-1,1), Pos(0,1), Pos(1,1))

  val numOccupied: Int = cells.values.count(_ == Occupied)

  def directNeighbours(pos: Pos): List[Cell] =
    offsets.map(off => cells.get(off.addition(pos))).flatten

  def occupiedDirectNeighbours(pos: Pos): Int = 
    directNeighbours(pos).count(_ == Occupied)

  def nextDirect: Area51 =
    copy(cells = cells.map((pos, cell) => 
      cell match
          case Occupied if occupiedDirectNeighbours(pos) >= 4 => (pos -> Empty)
          case Empty    if occupiedDirectNeighbours(pos) == 0 => (pos -> Occupied)
          case Occupied                                       => (pos -> Occupied)
          case Empty                                          => (pos -> Empty)
          case Floor                                          => (pos -> Floor)
          )
        )

  def findDirectStatic: Area51 =
    val nextArea = nextDirect
    if (nextArea == this) 
      nextArea
    else
      nextArea.findDirectStatic

  def distancedNeighbours(start: Pos): List[Cell] = 
    def recDN(current: Pos, dir: Pos): Option[Cell] = 
      cells.get(current.addition(dir)) match
       case Some(Floor)   => recDN(current.addition(dir), dir)
       case c: Some[Cell] => c
       case None          => None
    
    offsets.map(dir => recDN(start,dir)).flatten

  def occupiedDistantNeighbours(pos: Pos): Int = 
    distancedNeighbours(pos).count(_ == Occupied)

  def nextDistant: Area51 =
    copy(cells = cells.map((pos, cell) => 
      cell match
          case Occupied if occupiedDistantNeighbours(pos) >= 5 => (pos -> Empty)
          case Empty    if occupiedDistantNeighbours(pos) == 0 => (pos -> Occupied)
          case Occupied                                        => (pos -> Occupied)
          case Empty                                           => (pos -> Empty)
          case Floor                                           => (pos -> Floor)
          )
        )

  def findDistantStatic: Area51 =
    val nextArea = nextDistant
    if (nextArea == this) 
      nextArea
    else
      nextArea.findDistantStatic

  

object aoc112020 extends App:

  val input = Source.fromFile("src/main/resources/input_aoc11.txt").getLines.toList
  val (totalNumCells, map) = input
    .map(_.map(_ match
      case '#' => Occupied
      case 'L' => Empty
      case '.' => Floor).toList).flatten
    .foldLeft((0, Map[Pos, Cell]())) {
      case ((counter, map), cell) => (counter + 1, map + (Pos(counter % input(0).size, counter / input(0).size) -> cell))
    }

  println(Area51(map, input(0).size, input.size).findDistantStatic.numOccupied)

  val testInput = Source.fromFile("src/main/resources/input_aoc11.txt").getLines.toList
    .map(_.map(_ match
      case '#' => Occupied
      case 'L' => Empty
      case '.' => Floor).toList)


  //println(Area(input).findStatic.numOccupied)


// #.LL.L#.##
// #LLLLLL.L#
// L.L.L..L..
// #LLL.LL.L#
// #.LL.LL.LL
// #.LLLL#.##
// ..L.L.....
// #LLLLLLLL#
// #.LLLLLL.L
// #.#LLLL.##


// case class Area(rows: List[List[Cell]]):

//   override def toString: String =
//     rows.foldLeft("")((a,row) => a + "\n" + row.foldLeft("")((b, cell) => b + cell)) + "\n\n"

//   // assert (rows.size > 0 )
//   // assert (rows.forall(_.size > 0 ))

//   val offsets: List[(Int, Int)] =
//     List((-1,-1), (0,-1), (1,-1), (-1, 0), (1,0), (-1,1), (0,1), (1,1))

//   val maxX: Int = rows.size
//   val maxY: Int = rows(0).size

//   val numOccupied = rows.map(_.count(_ == Occupied)).sum

//   def get(row: Int, col: Int): Option[Cell] =
//     if ((row >= 0 && row < maxX ) && (col >= 0 && col < maxY))
//       Some(rows(row)(col))
//     else
//       None

//   def neighbours(row: Int, col: Int): List[Cell] =
//     offsets.map((x,y) => get(row + x, col + y)).flatten

//   def occupiedNeighbours(row: Int, col: Int): Int =
//     neighbours(row, col).count(_ == Occupied)

//   def next: Area =
//     val newRows = rows.zipWithIndex.foldLeft(List.empty: List[List[Cell]]) { case (newRows, (row, y)) =>
//       val newRow = row.zipWithIndex.foldLeft(List.empty: List[Cell]) { case (newCols, (pos, x)) =>
//         newCols :+ (pos match
//           case Occupied if occupiedNeighbours(y, x) >= 4 => Empty
//           case Empty    if occupiedNeighbours(y, x) == 0 => Occupied
//           case Occupied                                  => Occupied
//           case Empty                                     => Empty
//           case Floor                                     => Floor)

//       }
//       newRows :+ newRow }
//     copy(rows = newRows)

//   def findStatic: Area =
//     val nextArea = next
//     if (nextArea == this) 
//       nextArea
//     else
//       nextArea.findStatic
