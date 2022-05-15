package aoc2020

import scala.io.Source
import scala.util.{Success, Try}

def recursiveCheck(t: Long, ls: List[(Long,Long)], additive: Long): Long = 
  ls.find((id, index) => ((id - (t % id)) != index)) match
    case None => t
    case Some(_) => recursiveCheck(t + additive, ls, additive)
 


def chinese_remain(id: List[Long], index: List[Long]): Option[Long] = 
  assert(id.size == index.size)

  val product = id.product

  def iter(n: List[Long], a: List[Long], sm: Long): Long = 
    def mulInv(a: Long, b: Long): Long = 
      def loop(a: Long, b: Long, x0: Long, x1: Long): Long = 
        if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1

      if (b == 1) 1
      else 
        val x1 = loop(a, b, 0, 1)
        if (x1 < 0) x1 + b else x1

    if (n.nonEmpty) 
      val p = product / n.head

      iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)

      else 
        sm

  Try {
    (iter(id, index, 0) % product)
  } match {
    case Success(v) => Some(v)
    case _          => None
  }


object aoc132020 extends App:
  val List(time, busses) = Source.fromFile("src/main/resources/input_aoc13.txt").getLines.toList
  val bussesIDS = busses.split(",")
      .zip((0 until busses.length).reverse)
      .toList
      .map((bus, index) => (Try(bus.toLong).getOrElse(1: Long), index.toLong))
      .toMap 
  
  println("try solution " + (chinese_remain(bussesIDS.keys.toList, 
                                          bussesIDS.values.toList).get - busses.size + 1))
