package org.singingwizard.regexopt

sealed trait CharacterSelector {
}

case class CharacterSingle(c: Char) extends CharacterSelector {
  override def toString() = c.toString
}
case class CharacterRange(start: Char, end: Char) extends CharacterSelector {
  override def toString() = s"$start-$end"
}
case class CharacterUnparsed(s: String) extends CharacterSelector {
  override def toString() = s
}

sealed trait Regex extends PrecomputeHashcode {
  self: Product => 
  def cost: Int = 1
}
object Regex {
  private[regexopt] def greedyStr(b: Boolean) = if (b) "" else "?"
}

case class AnyCharacter() extends Regex {
  override def toString() = "."
}
case class StartOfString() extends Regex {
  override def toString() = "^"
}
case class EndOfString() extends Regex {
  override def toString() = "$"
}
case class Character(c: Char) extends Regex {
  override def toString(): String = {
    if (RegexParser.specials contains c)
      s"\\$c"
    else
      c.toString
  }
}
case class Star(r: Regex, greedy: Boolean = true) extends Regex {
  override def toString() = s"$r*${Regex.greedyStr(greedy)}"
  override def cost = r.cost * 5
}
case class Plus(r: Regex, greedy: Boolean = true) extends Regex {
  override def toString() = s"$r+${Regex.greedyStr(greedy)}"
  override def cost = r.cost * 5
}
case class Optional(r: Regex, greedy: Boolean = true) extends Regex {
  override def toString() = s"$r?${Regex.greedyStr(greedy)}"
  override def cost = r.cost * 2
}
case class Repetitions(r: Regex, n: Int, m: Int, greedy: Boolean = true) extends Regex {
  override def toString() = s"$r{$n,$m}${Regex.greedyStr(greedy)}"
  override def cost = r.cost * 5
}
case class Group(r: Regex, marker: String) extends Regex {
  override def toString() = {
    if (r.isInstanceOf[Alternative] && marker == "")
      r.toString
    else
      s"($marker$r)"
  }
  override def cost = r.cost
}
case class Escape(c: Char) extends Regex {
  override def toString() = s"\\$c"
  override def cost = 2
}
case class CharacterSet(cs: Set[CharacterSelector], complement: Boolean = false) extends Regex {
  override def toString() = s"[${if (complement) "^" else ""}${cs.mkString("")}]"
  override def cost = cs.size
}
case class Unparsed(s: String) extends Regex {
  override def toString() = s
  override def cost = s.size
}

class Alternative(val rs: Seq[Regex]) extends Regex with Product {
  override def toString() = "(" + rs.mkString("|") + ")"
  override def cost = rs.map(_.cost).sum + 1

  require(!rs.exists(_.isInstanceOf[Alternative]))

  override def equals(o: Any) = o match {
    case a: Alternative => a.rs == rs
    case _              => false
  }
  
  def canEqual(that: Any): Boolean = that.isInstanceOf[Alternative]   
  def productArity: Int = 1  
  def productElement(n: Int): Any = {
    require(n == 0)
    (rs: AnyRef).asInstanceOf[Any]
  }
}
object Alternative {
  def apply(a: Regex, b: Regex): Alternative = (a, b) match {
    case (Alternative(as), Alternative(bs)) => new Alternative(as ++ bs)
    case (Alternative(as), b)               => new Alternative(as :+ b)
    case (a, Alternative(bs))               => new Alternative(a +: bs)
    case (a, b)                             => new Alternative(Vector(a, b))
  }

  def apply(rs: Seq[Regex]): Alternative = new Alternative(rs.flatMap({
    case (Alternative(as)) => as
    case (a)               => Seq(a)
  }))

  def unapply(r: Alternative): Option[Seq[Regex]] = Some(r.rs)
}

case class Sequence(rs: Seq[Regex]) extends Regex {
  override def toString() = rs.mkString("")
  override def cost = rs.map(_.cost).sum

  require(!rs.exists(_.isInstanceOf[Sequence]))
}
object Sequence {
  def apply(a: Regex, b: Regex): Sequence = (a, b) match {
    case (Sequence(as), Sequence(bs)) => Sequence(as ++ bs)
    case (Sequence(as), b)            => Sequence(as :+ b)
    case (a, Sequence(bs))            => Sequence(a +: bs)
    case (a, b)                       => Sequence(Vector(a, b))
  }
}
