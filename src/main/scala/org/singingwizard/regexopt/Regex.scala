package org.singingwizard.regexopt

sealed trait CharacterSelector {
}

case class CharacterSingle(c: Char) extends CharacterSelector
case class CharacterRange(start: Char, end: Char) extends CharacterSelector
case class CharacterUnparsed(s: String) extends CharacterSelector

sealed trait Regex {
}

case class Any() extends Regex
case class StartOfString() extends Regex
case class EndOfString() extends Regex
case class Character(c: Char) extends Regex {
  override def toString(): String = c.toString()
}
case class Star(r: Regex, greedy: Boolean = true) extends Regex
case class Plus(r: Regex, greedy: Boolean = true) extends Regex
case class Optional(r: Regex, greedy: Boolean = true) extends Regex
case class Repetitions(r: Regex, n: Int, m: Int, greedy: Boolean = true) extends Regex
case class Group(r: Regex, marker: String) extends Regex
case class Escape(c: Char) extends Regex
case class CharacterSet(cs: Set[CharacterSelector], complement: Boolean = false) extends Regex
case class Unparsed(s: String) extends Regex

case class Alternative(rs: Seq[Regex]) extends Regex
object Alternative {
  def apply(a: Regex, b: Regex): Alternative = (a, b) match {
    case (Alternative(as), Alternative(bs)) => Alternative(as ++ bs)
    case (Alternative(as), b)               => Alternative(as :+ b)
    case (a, Alternative(bs))               => Alternative(a +: bs)
    case (a, b)                             => Alternative(Vector(a, b))
  }
}

case class Sequence(rs: Seq[Regex]) extends Regex
object Sequence {
  def apply(a: Regex, b: Regex): Sequence = (a, b) match {
    case (Sequence(as), Sequence(bs)) => Sequence(as ++ bs)
    case (Sequence(as), b)            => Sequence(as :+ b)
    case (a, Sequence(bs))            => Sequence(a +: bs)
    case (a, b)                       => Sequence(Vector(a, b))
  }
}