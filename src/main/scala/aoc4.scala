package aoc2020 
import scala.util.matching.Regex
import scala.io.Source

case class Passport(byr: Option[String], iyr: Option[String], eyr: Option[String], hgt: Option[String], 
                   hcl: Option[String], ecl: Option[String], pid: Option[String], cid: Option[String]) {
  def check: Boolean =
   (byr, iyr, eyr, hgt, hcl, ecl, pid) match
     case (Some(_), Some(_), Some(_), Some(_), Some(_), Some(_), Some(_)) => true
     case _ => false

  def valid: Boolean = 
    byr.map(byrValid).getOrElse(false) &
    iyr.map(iyrValid).getOrElse(false) &
    eyr.map(eyrValid).getOrElse(false) &
    hgt.map(hgtValid).getOrElse(false) &
    ecl.map(eclValid).getOrElse(false) 
}

def byrValid(s: String): Boolean =
  val year = s.substring(4).toInt 
  year >= 1920 & year <= 2002

def iyrValid(s: String): Boolean =
  val year = s.substring(4).toInt 
  year >= 2010 & year <= 2020

def eyrValid(s: String): Boolean =
  val year = s.substring(4).toInt 
  year >= 2020 & year <= 2030

def hgtValid(s: String): Boolean =
  s.substring(4) match
    case s if s.endsWith("cm") => val hgt = s.substring(0, 3).toInt; hgt >= 150 & hgt <= 193
    case s if s.endsWith("in") => val hgt = s.substring(0, 2).toInt; hgt >= 59 & hgt <= 76
    case _ => false

def eclValid(s: String): Boolean =
  List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(s.substring(4))

def stringToPassport(s: String): Passport =
   val byr = raw"byr:\d{4}".r.findFirstIn(s)
   val iyr = raw"iyr:\d{4}".r.findFirstIn(s)
   val eyr = raw"eyr:\d{4}".r.findFirstIn(s)
   val hgt = raw"hgt:(\d{3}cm|\d{2}in)".r.findFirstIn(s)
   val hcl = raw"hcl:#[0-9a-f]{6}".r.findFirstIn(s)
   val ecl = raw"ecl:[a-z]{3}".r.findFirstIn(s)
   val pid = "pid:[0-9]{9}(\\s|$)".r.findFirstIn(s)
   val cid = raw"cid:".r.findFirstIn(s)
   Passport(byr, iyr, eyr, hgt, hcl, ecl, pid, cid)

object AOC42020 extends App:
    println(Source
    .fromFile("src/main/resources/input_aoc4.txt")
    .mkString
    .split("\n\n")
    .map(_.map(_ match{ case '\n' => ' ' case x => x}))
    .map(stringToPassport(_))
    .filter(_.check)
    .filter(_.valid).length)