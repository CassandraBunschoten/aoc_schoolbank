package aoc2020

import scala.io.Source

def recursiveCheck(t: Long, ls: List[(Int,Int)], additive: Int): Long = 
  ls.find((id, index) => ((id - (t % id)) != index)) match
    case None => t
    case Some(_) => recursiveCheck(t + additive, ls, additive)
 
object aoc132020 extends App:
  val List(time, busses) = Source.fromFile("src/main/resources/testinput.txt").getLines.toList
  val bussesIDS  = busses.split(",").toList.zipWithIndex.filter((id, index) => id != "x").map((id, index) =>(id.toInt, index))

  println(recursiveCheck(0, bussesIDS, bussesIDS.head._1))
  println(bussesIDS)

  // val timeBus   = bussesIDS.map(b => b - (time.toInt % b) ).zip(bussesIDS)
  //val answer = timeBus.min._1 * timeBus.min._2
  //println(answer)
