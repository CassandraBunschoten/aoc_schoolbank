package aoc2020

import scala.io.Source

sealed trait Position:
  override def toString: String =
    this match
      case Occupied => "#"
      case Floor    => "."
      case Empty    => "L"
case object Occupied extends Position
case object Floor    extends Position
case object Empty    extends Position

case class Area(rows: List[List[Position]]):

  override def toString: String =
    rows.foldLeft("")((a,row) => a + "\n" + row.foldLeft("")((b,cell) => b + cell)) + "\n\n"

  // assert (rows.size > 0 )
  // assert (rows.forall(_.size > 0 ))

  val offsets: List[(Int, Int)] =
    List((-1,-1), (0,-1), (1,-1), (-1, 0), (1,0), (-1,1), (0,1), (1,1))

  val maxX: Int = rows.size
  val maxY: Int = rows(0).size

  val numOccupied = rows.map(_.count(_ == Occupied)).sum

  def get(row: Int, col: Int): Option[Position] =
    if ((row >= 0 && row < maxX ) && (col >= 0 && col < maxY))
      Some(rows(row)(col))
    else
      None
      
  def neighbours(row: Int, col: Int): List[Position] =
    offsets.map((x,y) => get(row + x, col + y)).flatten

  def occupiedNeighbours(row: Int, col: Int): Int =
    neighbours(row, col).foldLeft(0) {
      case (a, Occupied) => a + 1
      case (a, _)        => a
    }

  def next: Area =
    val newRows = rows.zipWithIndex.foldLeft(List.empty: List[List[Position]]) { case (newRows, (row, y)) =>
      val newRow = row.zipWithIndex.foldLeft(List.empty: List[Position]) { case (newCols, (pos, x)) =>
        newCols :+ (pos match 
          case Occupied if occupiedNeighbours(y, x) >= 4 => Empty
          case Empty    if occupiedNeighbours(y, x) == 0 => Occupied
          case Occupied                                  => Occupied
          case Empty                                     => Empty
          case Floor                                     => Floor)
      
      }
      newRows :+ newRow }
    copy(rows = newRows)

  def findStatic: Area =
    if (next == this) 
      next
    else
      next.findStatic


object AOC112020 extends App:
  val input = Source.fromFile("src/main/resources/input_aoc11.txt").getLines.toList
    .map(_.map(_ match 
      case '#' => Occupied
      case 'L' => Empty
      case '.' => Floor).toList)

  val testInput = Source.fromFile("src/main/resources/testinput.txt").getLines.toList
    .map(_.map(_ match 
      case '#' => Occupied
      case 'L' => Empty
      case '.' => Floor).toList)


  val testUneven = testInput.reverse.drop(1).reverse

  println(Area(testUneven).findStatic.numOccupied)

  println(Area(input).findStatic.numOccupied)


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