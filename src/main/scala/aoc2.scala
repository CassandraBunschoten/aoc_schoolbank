package aoc2020 
import scala.util.matching.Regex

import scala.io.Source

case class PasswordManager(int1: Int, int2: Int, delimiter: String, password: String){
    def validPassword: Boolean =
      val count = delimiter.r.findAllIn(password).length
      count >= int1 & count <= int2

    def validPassword3: Boolean = 
      val bool1 = raw".{${int1 - 1}}$delimiter.*".r.matches(this.password)
      val bool2 = raw".{${int2 - 1}}$delimiter.*".r.matches(this.password)
      bool1 != bool2

    def validPassword2: Boolean = 
      val bool1 = this.password.charAt(int1 - 1) == delimiter.toCharArray.head
      val bool2 = this.password.charAt(int2 - 1) == delimiter.toCharArray.head

      bool1 != bool2
}

object PasswordManager {
    def apply(s: String): PasswordManager =
      val pattern = """(\d+)[-](\d+)\s([a-z])[:]\s([a-zA-Z]*)""".r

      val pattern(min, max, delimiter, password) = s
      PasswordManager(min.toInt, max.toInt, delimiter, password)
}

object AOC22020 extends App:

    val input = Source
        .fromFile("fps/src/main/resources/input_aoc2.txt")
        .getLines
        .mkString

    val testinput = Source
        .fromFile("fps/src/main/resources/testinput.txt")
        .getLines
        .mkString

    val pattern = """(\d+)[-](\d+)\s([a-z])[:]\s([a-zA-Z]*)""".r

    println("Result 1: " + (pattern findAllIn input).map(PasswordManager(_)).map(_.validPassword).count(_ == true))
    println("Result 2: " + (pattern findAllIn input).map(PasswordManager(_)).map(_.validPassword3).count(_ == true))

    