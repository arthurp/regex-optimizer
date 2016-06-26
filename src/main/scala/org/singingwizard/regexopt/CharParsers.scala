package org.singingwizard.regexopt

import scala.util.parsing.combinator._

trait CharParsers extends Parsers with ImplicitConversions {
  implicit def listchar2string(l: List[Char]) = l.mkString("")
  implicit def listchar2stringFunc[T](f: String => T)(l: List[Char]) = f(l.mkString(""))
  implicit def string2ParserString(s: String) = accept(s.toList) ^^^ s

  type Elem = Char

  lazy val number = elem("digit", _.isDigit).+ ^^ (_.mkString("").toInt)

}