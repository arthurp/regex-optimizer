package org.singingwizard.regexopt

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object RegexParser extends CharParsers with PackratParsers {
  val specials = List(
    '.', '^', '$', '*', '+', '?', '{', '}', '(', ')', '[', ']', '|', '\\')

  lazy val character: PackratParser[Character] =
    (elem("normal character", (c: Char) => !(specials contains c)) ^^ Character) |
      ('\\' ~> elem("special character", (c: Char) => specials contains c) ^^ Character)

  lazy val special: PackratParser[Regex] =
    ('.' ^^^ Any()) |
      ('^' ^^^ StartOfString()) |
      ('$' ^^^ EndOfString()) |
      ('\\' ~> elem("character", (c: Char) => !(specials contains c)) ^^ Escape)

  lazy val combinator: PackratParser[Regex] =
    (regex <~ "*?" ^^ (Star(_, false))) |
      (regex <~ "+?" ^^ (Plus(_, false))) |
      (regex <~ "??" ^^ (Optional(_, false))) |
      (regex <~ '*' ^^ (Star(_, true))) |
      (regex <~ '+' ^^ (Plus(_, true))) |
      (regex <~ '?' ^^ (Optional(_, true))) |
      ((regex <~ '|') ~ regex ^^ (Alternative(_: Regex, _: Regex))) |
      (regex ~ ('{' ~> number <~ '}') ^^ ((r: Regex, n: Int) => Repetitions(r, n, n, true))) |
      ((regex ~ ('{' ~> number)) ~ (',' ~> number <~ '}') ^^ ((r: Regex, n: Int, m: Int) => Repetitions(r, n, m, true))) |
      ((regex ~ ('{' ~> number)) ~ (',' ~> number <~ "}?") ^^ ((r: Regex, n: Int, m: Int) => Repetitions(r, n, m, false)))

  lazy val other: PackratParser[Regex] =
    ("[^" ~> elem("character", _ != ']').+ <~ ']' ^^ ((s: List[Char]) => CharacterSet(Set(CharacterUnparsed(s)), true))) |
      ('[' ~> elem("character", _ != ']').+ <~ ']' ^^ ((s: List[Char]) => CharacterSet(Set(CharacterUnparsed(s)))))

  def unparsed(p: Char => Boolean) = elem("character", p).+ ^^ Unparsed

  lazy val group: PackratParser[Regex] =
    "(?" ~> elem("character", c => !"()".contains(c)).+ <~ ")" ^^ ((s: String) => Unparsed(s"(?$s)")) |
      "(" ~> regex <~ ")" ^^ (Group(_, ""))

  lazy val component: PackratParser[Regex] =
    combinator |
      group |
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
  def parseExactMatch(s: String) =
    Sequence(s.toSeq.map(Character))
}