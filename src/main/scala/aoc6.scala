package aoc2020

import scala.io.Source

def getDup(group: List[String]): List[Char] =
  val chars = ('a' to 'z').toList
  group.foldLeft(chars)((acc, elm) => acc.filter(elm.contains(_)))

def getLetters(group: List[String]): List[Char] =
  group.foldLeft(List(): List[Char])((acc,elm) => elm.toCharArray.toList ::: acc).distinct

object AOC62020 extends App:
  val groups = Source
    .fromFile("src/main/resources/input_aoc6.txt")
    .mkString
    .split("\n\n")
    .map(_.split("\n").toList)
    .map(getDup(_).length).sum

  println(groups)
