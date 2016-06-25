package org.singingwizard.regexopt

object ManualTest extends App {
  println(RegexParser.parse("a\\a"))
  println(RegexParser.parseOpt("(?abcd)"))
}