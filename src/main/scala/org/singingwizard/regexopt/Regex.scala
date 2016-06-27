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

sealed trait Regex {
}
object Regex {
  private[regexopt] def greedyStr(b: Boolean) = if(b) "" else "?"
}

case class Any() extends Regex {
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
}
case class Plus(r: Regex, greedy: Boolean = true) extends Regex {
  override def toString() = s"$r+${Regex.greedyStr(greedy)}"
}
case class Optional(r: Regex, greedy: Boolean = true) extends Regex {
  override def toString() = s"$r?${Regex.greedyStr(greedy)}"
}
case class Repetitions(r: Regex, n: Int, m: Int, greedy: Boolean = true) extends Regex {
  override def toString() = s"$r{$n,$m}${Regex.greedyStr(greedy)}"
}
case class Group(r: Regex, marker: String) extends Regex {
  override def toString() = {
    if (r.isInstanceOf[Alternative] && marker == "")
      r.toString
    else
      s"($marker$r)"
  }
}
case class Escape(c: Char) extends Regex {
  override def toString() = s"\\$c"
}
case class CharacterSet(cs: Set[CharacterSelector], complement: Boolean = false) extends Regex {
  override def toString() = s"[${if (complement) "^" else ""}${cs.mkString("")}]"
}
case class Unparsed(s: String) extends Regex {
  override def toString() = s
}

case class Alternative(rs: Seq[Regex]) extends Regex {
  override def toString() = "(" + rs.mkString("|") + ")"
}
object Alternative {
  def apply(a: Regex, b: Regex): Alternative = (a, b) match {
    case (Alternative(as), Alternative(bs)) => Alternative(as ++ bs)
    case (Alternative(as), b)               => Alternative(as :+ b)
    case (a, Alternative(bs))               => Alternative(a +: bs)
    case (a, b)                             => Alternative(Vector(a, b))
  }
}

case class Sequence(rs: Seq[Regex]) extends Regex {
  override def toString() = rs.mkString("")
}
object Sequence {
  def apply(a: Regex, b: Regex): Sequence = (a, b) match {
    case (Sequence(as), Sequence(bs)) => Sequence(as ++ bs)
    case (Sequence(as), b)            => Sequence(as :+ b)
    case (a, Sequence(bs))            => Sequence(a +: bs)
    case (a, b)                       => Sequence(Vector(a, b))
  }
}

object RegexGraph {
  def apply(r: Regex): Graph[Regex] = {
    case class TState(startNode: Graph.Node, endNode: Graph.Node, graph: Graph[Regex])
    
    def process(r: Regex, s: TState): Graph[Regex] = {
      r match {
        case Sequence(rs) => {
          val TState(start, end, g) = rs.foldLeft(s) { (s, r) =>
            val next = new Graph.Node()
            val g = s.graph + next
            s.copy(graph = process(r, s.copy(graph = g, endNode = next)), startNode = next)
          }
          val merged = g.merge(start, s.endNode)
          merged
        }
        case Alternative(rs) => {
          val TState(start, end, _) = s
          rs.foldLeft(s.graph) { (g, r) =>
            process(r, TState(start, end, g))
          }
        }
        case Group(r, "") => {
          process(r, s)
        }
        case _ => s.graph + s.graph.Edge(s.startNode, r, s.endNode)
      }
    }
    
    val start = new Graph.Node()
    start.setName("start")
    val end = new Graph.Node()
    end.setName("end")
    val g = Graph[Regex]() + start + end
    
    process(r, TState(start, end, g))
  }
  def unapply(r: Graph[Regex]): Regex = {
    /* Remove nodes one at a time by creating edges that are labeled with the 
     * concatenation of the edges in and out of the removed node. The edge is
     * combined with the existing edges using alternative.  
     * 
     * This is correct however the quality of the result is extremely sensetive
     * to the order in which nodes are removed.
     * 
     * It is not clear what heuristic will provide useful results.
     */
    ???
  }
}