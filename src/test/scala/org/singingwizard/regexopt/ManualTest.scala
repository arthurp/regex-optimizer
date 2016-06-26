package org.singingwizard.regexopt
import scala.io.Source

object ManualTest {

  def main(args: Array[String]) = {
    println(RegexParser.parse("a\\a"))
    println(RegexParser.parseOpt("(?abcd)"))
  }
}