package org.singingwizard.regexopt

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object GlobParser extends CharParsers with PackratParsers {
  val specials = List(
    '*', '?', '[', ']', '\\')

  lazy val character: PackratParser[Character] =
    (elem("normal character", (c: Char) => !(specials contains c)) ^^ Character) |
      ('\\' ~> elem("special character", (c: Char) => specials contains c) ^^ Character)

  lazy val special: PackratParser[Regex] =
    ("?" ^^^ Any()) |
      ("**" ^^^ Star(Any())) |
      ("*" ^^^ Star(CharacterSet(Set(CharacterSingle('/')), true))) |
      ('\\' ~> elem("character", (c: Char) => !(specials contains c)) ^^ Escape)

  lazy val other: PackratParser[Regex] =
    ('[' ~> elem("character", _ != ']').+ <~ ']' ^^ ((s: List[Char]) => CharacterSet(Set(CharacterUnparsed(s)))))

  lazy val component: PackratParser[Regex] =
    character |
      special |
      other

  lazy val regex: PackratParser[Regex] =
    component ~ regex ^^ (Sequence(_: Regex, _: Regex)) |
      component //|
  //unparsed(_ => true)

  def parseOpt(s: String) =
    phrase(regex)(new CharSequenceReader(s))
  def parse(s: String) = parseOpt(s).get
}